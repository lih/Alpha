{-# LANGUAGE ViewPatterns, ImplicitParams, NoMonomorphismRestriction, ParallelListComp, TupleSections, CPP, ForeignFunctionInterface, MultiParamTypeClasses, RankNTypes #-}
module Main(
  -- * The Main function
  main,

  -- * Behaviour
  printHelp,printVersion,interactive,

  -- ** Compiling
  compileProgram,
  compileLanguage,
  compileExpr,doTransform,

  -- * Environment
  importLanguage,
  findSource,languageFile,destFile,entry,
  contextRef,withInitialContext,initialContext,
  
  -- ** JIT
  compAddrRef,getAddress,getAddressJIT,getAddressComp,

  exportAlpha,internCode,

  runSym,callSym,enableExec,
  
  -- ** Bindings
  initialBindings,
  
  alpha_compAddr,alpha_setTransform,

  alpha_symName,alpha_nameSym,alpha_createSym,
  alpha_numSym,

  alpha_allocate,alpha_free,

  alpha_printVal,alpha_printHelp,alpha_printOK,alpha_printNum,alpha_printList,
  alpha_printLang,alpha_printExpr,

  alpha_import,alpha_reload,

  -- * Wrappers
  mprotect, mkProc, mkFunSize, mkFunInit, mkFunTransform,

  -- * Utilities
  funPtrToInteger,pageSize,
  pok,pik,

  modTime,withRef,

  -- * Instances
  IO(..),Str(..),
  Storable(..)
  ) where

import Architectures
import Bindings.Posix.Sys.Mman
import Bindings.Posix.Unistd
import Compile
import Context as C
import Control.Monad.State.View
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Version
import Foreign hiding (unsafePerformIO,unsafeForeignPtrToPtr,void,new)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Format
import ID
import My.Control.Monad
import Misc (tagIO,ifThenElse,mkIORef)
import PCode
import Paths_alpha (version)
import Safe
import Settings
import Specialize
import Syntax
import Syntax.Parse
import System.Console.Readline (readline,addHistory)
import System.Directory
import System.Environment as SE
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
import qualified Data.Bimap as BM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Serialize as Ser

main = do
  args <- getArgs
  case getSettings args of
    Right s -> putSettings s >> do
      withSettings $ case (action ?settings,programs ?settings) of
        (PrintHelp,_)    -> printHelp
        (PrintVersion,_) -> printVersion
        (Compile,[])     -> interactive
        (Compile,progs)  -> mapM_ compileProgram progs
    Left err -> fail err

printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++showVersion version
interactive = withInitialContext $ do
  putStrLn $ "Alpha, version "++showVersion version++". Type alpha/help() for help on Alpha invocation. ^D to exit."
  readAll
  putStrLn "\rGoodbye !"
  where readAll = readMulti "alpha> " >>= \_l -> case _l of
          Just l -> do
            addHistory l
            mapM_ compileExpr exprs
            readAll
            where exprs = parseAlpha "/dev/stdin" l
          Nothing -> return ()
        readMulti p = readline p >>= \_l -> case _l of
          Just l | lastMay l==Just '\\' -> readMulti " ... > " >>= \n -> return (Just (init l++"\n") <> n)
                 | otherwise -> return (Just l)
          Nothing -> return Nothing

compileProgram (lang,entryName) = withInitialContext $ do
  skipLink <- fileExist (destFile entryName) <&&> ((<=) <$> importLanguage (const $ return ()) lang <*> modTime entryName)
  unless skipLink $ tagIO ("Linking program "++entryName) $ do
    entrySym <- viewState language_ $ internSym entryName
    _ <- getAddressComp (outputArch ?settings) entrySym
    (addrs,ptrs) <- unzip <$> sortBy (comparing fst) <$> M.elems <$> gets compAddresses
    top <- gets compTop
    contents <- B.concat <$> sequence [withForeignPtr ptr $ \p -> unsafePackCStringLen (castPtr p,size)
                                   | ptr <- ptrs | size <- zipWith (-) (tail addrs++[top]) addrs]
    writeFormat (outputFmt ?settings) (destFile entryName) contents
compileLanguage force name = do
  let langFile = languageFile name
      newerThan f1 f2 = (>=) <$> modTime f1 <*> modTime f2
  source <- findSource name
  skip <- return (not force) <&&> fileExist langFile <&&> maybe (return True) (langFile `newerThan`) source
  l <- if skip then either (\e -> error $ "Error reading language file "++langFile++": "++e) id
                    <$> Ser.decode <$> B.readFile langFile else do
         tagIO ("Compiling language "++name) $ do
           lang <- compileFile $ fromMaybe (error $ "Couldn't find source file for language "++name) source
           createDirectoryIfMissing True (dropFileName langFile)
           B.writeFile langFile (Ser.encode lang)
           return lang
  time <- modTime langFile
  return (time,l)
    where compileFile src = withInitialContext $ do
            str <- readFile src
            loaders <- mapM compileExpr (parseAlpha src str)
            gets (language >>> purgeLanguage >>> (,loaders))
compileExpr expr = do
  expr' <- doTransform =<< viewing language_ (envCast expr)
  code <- viewing language_ $ compile [] Nothing expr'
  internCode code >>= \s -> runSym s >> return s
doTransform syn = gets transform >>= ($syn) . maybe return tr 
  where tr fun tree = do
          root <- new tree
          newTree <- unsafeUseAsCString initStub $ \stub -> mkFunTransform (castPtrToFunPtr stub) fun (castPtr root)
          peek (castPtr newTree) :: IO (Syntax ID)

importLanguage loadImport = _import
  where _import lang = gets language >>= \l -> ifThenElse (lang`isImport`l) (modTime $ languageFile lang) $ do
          node@(time,(l',_)) <- compileLanguage False lang
          times <- mapM _import (getImports l')
          loadLang lang =<< if any (> time) times then compileLanguage True lang else return node
        loadLang lang (time,(l,load)) = do
          let l' = l{ nameL = lang }
          load' <- viewing language_ $ modify (<>l') >> gets (translateInit load l')
          loadImport load'
          return time
findSource l = findM doesFileExist [dir</>l<.>"a" | dir <- sourceDirs ?settings]
languageFile l = languageDir ?settings</>l<.>"l"
destFile n = destDir ?settings</>n 
entry = formatEntry $ outputFmt ?settings
contextRef = mkIORef (error "Undefined context" :: Context)
instance MonadState Context IO where
  get = readIORef contextRef
  put = writeIORef contextRef
withInitialContext = swapping id_ initialContext
initialContext = C lang jitA M.empty (fromIntegral entry) Nothing
  where (lang,jitA) = execState (mapM_ st initialBindings) (mempty,M.empty)
          where st (s,_v) = do
                  i <- viewState fst_ (internSym s)
                  case _v of
                    Left v -> modifying fst_ (setSymVal i v)
                    Right p -> modifying snd_ (M.insert i p)

compAddrRef = mkIORef (undefined :: ID -> IO Int)
getAddress arch lookupAddr register = withRef compAddrRef getAddr . getAddr
  where
    getAddr sym = lookupAddr sym >>= \val -> case val of
      Just a -> return a
      Nothing -> gets language >>= \lang -> (>> getAddr sym) $ case lookupSymVal sym lang of
        Verb c -> void $ do
          let (size,codem) = specialize arch (sym,getAddr) c
          ptr <- mallocForeignPtrBytes size
          register sym ptr size
          code <- codem
          withForeignPtr ptr $ \p -> do
            unsafeUseAsCStringLen code $ \(p',n) -> copyBytes p (castPtr p') n
            enableExec p size
        Noun sizeCode initCode -> do
          [sizeSym,initSym] <- mapM internCode [sizeCode,initCode]
          size <- callSym mkFunSize execStub sizeSym id
          ptr <- mallocForeignPtrBytes size
          register sym ptr size
          withForeignPtr ptr $ \p -> callSym mkFunInit initStub initSym ($castPtr p)
        _ -> fail $ "Couldn't find definition of symbol "++fromMaybe (show sym) (lookupSymName sym lang)
getAddressJIT = getAddress arch_host lookupAddr register
  where lookupAddr sym = do
          val <- M.lookup sym <$> gets jitAddresses
          return $ (fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr) <$> val
        register sym ptr _ = modifying jitAddresses_ (M.insert sym ptr)
getAddressComp arch = getAddress arch lookupAddr register
  where lookupAddr sym = (fst<$>) <$> M.lookup sym <$> gets compAddresses
        register sym ptr size = do
          n <- getting compTop_
          modifying compAddresses_ (M.insert sym (n,ptr))
          modifying compTop_ (+size)

exportAlpha stub ptr = unsafePerformIO $ do
  unsafeUseAsCStringLen (stub $ funPtrToInteger ptr) $ \(src,size) -> do
    _ptr <- mallocForeignPtrBytes size
    withForeignPtr _ptr $ \dst -> copyBytes (castPtr dst) src size
    return _ptr
internCode code = viewing language_ $ state createSym >>= \s -> modify (exportSymVal s (Verb code)) >> return s

callSym :: (FunPtr (Ptr() -> a) -> Ptr() -> a) -> ByteString -> ID -> (a -> IO b) -> IO b
callSym wrap _stub sym f = unsafeUseAsCString _stub $ \stub -> do
  p <- getAddressJIT sym
  f $ wrap (castPtrToFunPtr stub) (intPtrToPtr $ fromIntegral p)
runSym sym = callSym mkProc execStub sym id
enableExec p size = do
  let p' = alignPtr (p`plusPtr`(1-pageSize)) pageSize
  mprotect (castPtr p') (fromIntegral $ size+ p`minusPtr`p') (c'PROT_READ .|. c'PROT_WRITE .|. c'PROT_EXEC)

initialBindings = [(n,Left $ Builtin b) | (b,n) <- bNames] ++ [
  ("alter"  ,Left $ Axiom XAlter),
  ("bind"   ,Left $ Axiom XBind),

  ("choose" ,Left $ Axiom XChoose),
  ("<-"     ,Left $ Axiom XRestart),
  ("->"     ,Left $ Axiom XReturn),
  ("do"     ,Left $ Axiom XDo),

  ("@"      ,Left $ Axiom XAddr),
  ("#"      ,Left $ Axiom XSize),

  ("id"     ,Left $ Axiom XID),
  ("verb"   ,Left $ Axiom XVerb),
  ("noun"   ,Left $ Axiom XNoun),
  ("lang"   ,Left $ Axiom XLang)] ++ [

  ("alpha/c@"            , Right $ exportAlpha callStub1 alpha_compAddr),    
  ("alpha/create-symbol" , Right $ exportAlpha callStub0 alpha_createSym),
  ("alpha/number-symbol" , Right $ exportAlpha callStub1 alpha_numSym),
  ("alpha/symbol-name"   , Right $ exportAlpha callStub1 alpha_symName),
  ("alpha/name-symbol"   , Right $ exportAlpha callStub1 alpha_nameSym),

  ("alpha/set-transform" , Right $ exportAlpha callStub1 alpha_setTransform),    
  ("alpha/import"        , Right $ exportAlpha callStub1 alpha_import),
  ("alpha/reload"        , Right $ exportAlpha callStub0 alpha_reload),    
  
  ("alpha/allocate"      , Right $ exportAlpha callStub1 alpha_allocate), 
  ("alpha/free"          , Right $ exportAlpha callStub1 alpha_free), 
  
  ("alpha/help"          , Right $ exportAlpha callStub0 alpha_printHelp),

  -- debugging functions
  ("alpha/list"          , Right $ exportAlpha callStub0 alpha_printList),
  ("alpha/lang"          , Right $ exportAlpha callStub0 alpha_printLang),
  ("alpha/print-OK"      , Right $ exportAlpha callStub0 alpha_printOK),    
  ("alpha/print-num"     , Right $ exportAlpha callStub1 alpha_printNum),
  ("alpha/print-expr"    , Right $ exportAlpha callStub1 alpha_printExpr),
  ("alpha/print-val"     , Right $ exportAlpha callStub1 alpha_printVal)
  ]
#define str(x) #x
#define ALPHA_EXPORT(fun,t) foreign export ccall str(__alpha_##fun) __alpha_##fun :: t ; foreign import ccall str(&__alpha_##fun) alpha_##fun :: FunPtr (t) ; __alpha_##fun
ALPHA_EXPORT(compAddr,ID -> IO Int) sym = readIORef compAddrRef >>= ($sym)
ALPHA_EXPORT(setTransform,Ptr () -> IO ()) fun = modify $ \c -> c { transform = Just fun }

ALPHA_EXPORT(symName,ID -> IO (Ptr Word8)) sym = do
  n <- gets (lookupSymName sym . language)
  ret <- newArray0 0 (map c2w $ fromMaybe "" n)
  return ret
ALPHA_EXPORT(nameSym,Ptr Word8 -> IO ID) p = do
  l <- peekArray0 0 p
  viewState language_ (internSym $ map w2c l)
ALPHA_EXPORT(createSym,IO ID) = viewState language_ createSym
ALPHA_EXPORT(numSym,Int -> IO ID) = viewState language_ . internSym . show

ALPHA_EXPORT(allocate,Int -> IO (Ptr())) = mallocBytes
ALPHA_EXPORT(free,Ptr() -> IO ()) = free

ALPHA_EXPORT(printVal,ID -> IO ()) sym = gets (language >>> lookupSymVal sym) >>= print
ALPHA_EXPORT(printHelp,IO()) = printHelp
ALPHA_EXPORT(printOK,IO()) = putStrLn "OK"
ALPHA_EXPORT(printNum,Int -> IO()) n = print (intPtrToPtr $ fromIntegral n)
ALPHA_EXPORT(printList,IO()) = do
  syms <- getting (language_ >>> syms_)
  putStrLn $ intercalate " " (map fst $ BM.toList syms)
ALPHA_EXPORT(printLang,IO()) = getting language_ >>= print
ALPHA_EXPORT(printExpr,Ptr() -> IO()) = peek . castPtr >=> _transform >=> print
  where _transform t = do
          smap <- getting (language_ >>> syms_)
          let symName s = fromMaybe (show s) $ BM.lookupR s smap
          return (fmap (Str . symName) (t :: Syntax ID))

ALPHA_EXPORT(import,ID -> IO()) sym = withSettings $ do
  _name <- getting (language_ >>> f_ (lookupSymName sym))
  void $ case _name of
    Just name -> importLanguage (mapM_ runSym) name
    Nothing -> error $ "Symbol "++show sym++" has no name."
ALPHA_EXPORT(reload,IO()) = withSettings $ do
  imports <- getting (language_ >>> f_ (languagesL >>> BM.toList >>> map fst)) 
  put initialContext
  mapM_ (importLanguage (mapM_ runSym)) imports


-- |The mprotect function in "System.Posix.Mman" doesn't have the right type signature, so we reimport it here
mprotect :: Ptr () -> CSize -> CInt -> IO CInt
foreign import ccall "mprotect" mprotect :: Ptr () -> CSize -> CInt -> IO CInt
#define DYNAMIC(name,type) foreign import ccall "dynamic" name :: FunPtr (type) -> type
DYNAMIC(mkProc,Ptr() -> IO ())
DYNAMIC(mkFunSize,Ptr() -> IO Int)
DYNAMIC(mkFunInit,Ptr() -> Ptr() -> IO ())
DYNAMIC(mkFunTransform,Ptr() -> Ptr() -> IO (Ptr()))

funPtrToInteger f = fromIntegral $ ptrToIntPtr $ castFunPtrToPtr f
pageSize = fromIntegral $ unsafePerformIO $ c'sysconf c'_SC_PAGESIZE
pok e p = poke (castPtr p) e >> return (p`plusPtr`sizeOf e)
pik p = peek (castPtr p) >>= \e -> return (e,p`plusPtr`sizeOf e)

modTime f = modificationTime <$> getFileStatus f
withRef ref val _x = readIORef ref >>= \v -> writeIORef ref val >> _x >>= \x -> writeIORef ref v >> return x

intSize = sizeOf (undefined::Int)
ptrSize = sizeOf (undefined::Ptr())
instance Storable ID where
  alignment = alignment . unID
  sizeOf = sizeOf . unID
  peek = fmap ID . peek . castPtr
  poke p = poke (castPtr p) . unID
instance Storable a => Storable (Syntax a) where
  alignment = const (alignment (undefined::Ptr ()))
  sizeOf (Group g) = intSize + intSize + ptrSize*length g
  sizeOf (Symbol s) = intSize + sizeOf s
  peek p = pik p >>= \(t,p') -> case (t::Int) of
    0 -> p' § (pik >=> uncurry peekArray >=> fmap Group . mapM peek)
    1 -> Symbol <$> peek p'
    _ -> fail "ill-formed tree (prefix should be 0 or 1)"
  poke = flip poke_
    where poke_ (Group g) = pok (0::Int) >=> pok (length g) >=> \p -> do
            mapM new g >>= \l -> pokeArray (castPtr p) l
          poke_ (Symbol s) = pok (1::Int) >=> pok s >=> const (return ())

new s = mallocBytes (sizeOf s) ->>= flip poke s

newtype Str = Str String
instance Show Str where show (Str s) = s


            
