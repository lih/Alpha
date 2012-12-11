{-# LANGUAGE ViewPatterns, ImplicitParams, NoMonomorphismRestriction, ParallelListComp, TupleSections, CPP, ForeignFunctionInterface, MultiParamTypeClasses, RankNTypes #-}
import Bindings.Posix.Sys.Mman
import Bindings.Posix.Unistd
import Compile
import Context as C
import Control.Category ((>>>))
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Version
import Foreign hiding (unsafePerformIO,unsafeForeignPtrToPtr,void)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Format
import ID
import My.Control.Monad
import My.Control.Monad.State
import My.Prelude
import Options
import PCode
import Paths_alpha (version)
import Serialize
import Specialize
import Specialize.Architecture
import Syntax
import Syntax.Parse
import System.Directory
import System.Environment as SE
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
import qualified Data.Bimap as BM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Serialize as Ser

-- The mprotect function in System.Posix.Mman doesn't have the right type signature, so we reimport it here
foreign import ccall "mprotect" mprotect :: Ptr () -> CSize -> CInt -> IO CInt

newtype Str = Str String
instance Show Str where show (Str s) = s

withSettings :: ((?settings :: Settings) => IO a) -> IO a
withSettings m = gets settings >>= \s -> let ?settings = s in m

main = do
  args <- getArgs
  case getSettings args of
    Right s -> do
      put (C s undefined undefined undefined undefined undefined)
      withSettings $ case (action ?settings,programs ?settings) of
        (PrintHelp,_)    -> printHelp
        (PrintVersion,_) -> printVersion
        (Compile,[])     -> interactive
        (Compile,progs)  -> mapM_ compileProgram progs
    Left err -> fail err

printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++showVersion version

entry = formatEntry $ outputFmt ?settings
languageFile language = languageDir ?settings</>language<.>"l"
findSource language = findM doesFileExist [dir</>language<.>"a" | dir <- sourceDirs ?settings]

interactive = withInitialContext $ do
  putStrLn $ "Alpha, version "++showVersion version++". Type alpha/help() for help on Alpha invocation. ^D to exit."
  str <- getContents
  putStr "> " >> hFlush stdout
  mapM_ (\e -> compileExpr e >> putStr "> " >> hFlush stdout) (parseAlpha "/dev/stdin" str)
  putStrLn "\rGoodbye !"
compileProgram (language,entryName) = withInitialContext $ do
  importLanguage compileLanguage (const $ return ()) language
  entrySym <- viewState language_ $ internSym entryName
  _ <- getAddressComp (outputArch ?settings) entrySym
  (addrs,ptrs) <- unzip $< sortBy (comparing fst) $< M.elems $< gets compAddresses
  top <- gets compTop
  contents <- B.concat $< sequence [withForeignPtr ptr $ \p -> unsafePackCStringLen (castPtr p,size)
                                   | ptr <- ptrs | size <- zipWith (-) (tail addrs++[top]) addrs]
  writeFormat (outputFmt ?settings) entryName contents
compileLanguage force name = do
  let langFile = languageFile name
  source <- findSource name
  skip <- return (not force) <&&> fileExist langFile <&&> maybe (return True) (langFile `newerThan`) source
  l <- if skip then either (\e -> error $ "Error reading language file "++langFile++": "++e)
                    id $< Ser.decode $< B.readFile langFile else do
         putStrLn $ "Compiling language "++name++"..."
         lang <- compileFile $ fromMaybe (error $ "Couldn't find source file for language "++name) source
         createDirectoryIfMissing True (dropFileName langFile)
         B.writeFile langFile (Ser.encode lang)
         return lang
  return (not skip,l)
    where compileFile src = withInitialContext $ do
            str <- readFile src
            inits <- mapM compileExpr (parseAlpha src str)
            getting (language_ >>> f_ (purgeLanguage >>> (,inits)))
compileExpr expr = do
  expr <- doTransform =<< viewing language_ (envCast expr)
  code <- viewing language_ $ compile [] Nothing expr
  internCode code >>= \s -> runSym s >> return s

internCode code = viewing language_ $ state createSym >>= \s -> modify (exportSymVal s (Verb code)) >> return s

#define DYNAMIC(name,type) foreign import ccall "dynamic" name :: FunPtr (type) -> type
DYNAMIC(mkProc,Ptr() -> IO ())
DYNAMIC(mkFunSize,Ptr() -> IO Int)
DYNAMIC(mkFunInit,Ptr() -> Ptr() -> IO ())
DYNAMIC(mkFunTransform,Ptr() -> Ptr() -> IO (Ptr()))

funPtrToInteger f = fromIntegral $ ptrToIntPtr $ castFunPtrToPtr f
exportAlpha stub ptr = unsafePerformIO $ do
  unsafeUseAsCStringLen (stub $ funPtrToInteger ptr) $ \(src,size) -> do
    ptr <- mallocForeignPtrBytes size
    withForeignPtr ptr $ \dst -> copyBytes (castPtr dst) src size
    return ptr
initialBindings = [(n,Left $ Builtin b) | (b,n) <- bNames] ++ [
  ("alter"  ,Left $ Axiom XAlter),
  ("bind"   ,Left $ Axiom XBind),

  ("choose" ,Left $ Axiom XChoose),
  ("<-"     ,Left $ Axiom XRestart),
  ("->"     ,Left $ Axiom XReturn),
  ("do"     ,Left $ Axiom XDo),

  ("verb"   ,Left $ Axiom XVerb),
  ("noun"   ,Left $ Axiom XNoun),

  ("id"     ,Left $ Axiom XID),
  ("@"      ,Left $ Axiom XAddr),
  ("#"      ,Left $ Axiom XSize)] ++ [

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
  ("alpha/print-num"     , Right $ exportAlpha callStub1 alpha_printNum)
  ]

#define str(x) #x
#define ALPHA_EXPORT(fun,t) foreign export ccall str(__alpha_##fun) __alpha_##fun :: t ; foreign import ccall str(&__alpha_##fun) alpha_##fun :: FunPtr (t) ; __alpha_##fun
ALPHA_EXPORT(setTransform,Ptr () -> IO ()) fun = modify $ \c -> c { transform = Just fun }
ALPHA_EXPORT(compAddr,ID -> IO Int) id = readIORef compAddrRef >>= ($id)

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

ALPHA_EXPORT(printHelp,IO()) = printHelp
ALPHA_EXPORT(printOK,IO()) = putStrLn "OK"
ALPHA_EXPORT(printNum,Int -> IO()) n = print (intPtrToPtr $ fromIntegral n)
ALPHA_EXPORT(printList,IO()) = do
  syms <- getting (language_ >>> syms_)
  putStrLn $ intercalate " " (map fst $ BM.toList syms)
ALPHA_EXPORT(printLang,IO()) = getting language_ >>= print

ALPHA_EXPORT(import,ID -> IO()) sym = withSettings $ do
  name <- getting (language_ >>> f_ (lookupSymName sym))
  void $ case name of
    Just name -> importLanguage compileLanguage (mapM_ runSym) name
    Nothing -> error $ "Symbol "++show sym++" has no name."
ALPHA_EXPORT(reload,IO()) = withSettings $ do
  imports <- getting (language_ >>> f_ (languagesL >>> BM.toList >>> map fst)) 
  put initialContext
  mapM_ (importLanguage compileLanguage (mapM_ runSym)) imports

compAddrRef = unsafePerformIO $ newIORef (undefined :: ID -> IO Int)
contextRef = unsafePerformIO $ newIORef (error "Undefined context" :: Context)
instance MonadState Context IO where
  get = readIORef contextRef
  put = writeIORef contextRef

withInitialContext = withState initialContext
initialContext = C ?settings lang jitA M.empty (fromIntegral entry) Nothing
  where (lang,jitA) = execState (mapM_ st initialBindings) (mempty,M.empty)
          where st (s,v) = do
                  i <- viewState fst_ (internSym s)
                  case v of
                    Left v -> modifying fst_ (setSymVal i v)
                    Right p -> modifying snd_ (M.insert i p)


pageSize = fromIntegral $ unsafePerformIO $ c'sysconf c'_SC_PAGESIZE
enableExec p size = do
  let p' = alignPtr (p`plusPtr`(1-pageSize)) pageSize
  mprotect (castPtr p') (fromIntegral $ size+ p`minusPtr`p') (c'PROT_READ .|. c'PROT_WRITE .|. c'PROT_EXEC)
  
callSym :: (FunPtr (Ptr() -> a) -> Ptr() -> a) -> ByteString -> ID -> (a -> IO b) -> IO b
callSym wrap stub sym f = unsafeUseAsCString stub $ \stub -> do
  p <- getAddressJIT sym
  f $ wrap (castPtrToFunPtr stub) (intPtrToPtr $ fromIntegral p)
runSym sym = callSym mkProc execStub sym id

withRef ref val x = readIORef ref >>= \v -> writeIORef ref val >> x >>= \x -> writeIORef ref v >> return x
getAddress arch lookup register = withRef compAddrRef getAddr . getAddr
  where
    getAddr sym = lookup sym >>= \val -> case val of
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

getAddressJIT = getAddress arch_host lookup register
  where lookup id = do
          val <- M.lookup id $< gets jitAddresses
          return $ (fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr) $< val
        register id ptr size = modifying jitAddresses_ (M.insert id ptr)
getAddressComp arch = getAddress arch lookup register
  where lookup id = (fst$<) $< M.lookup id $< gets compAddresses
        register id ptr size = do
          n <- getting compTop_
          modifying compAddresses_ (M.insert id (n,ptr))
          modifying compTop_ (+size)

doTransform syn = gets transform >>= ($syn) . maybe return tr 
  where tr fun tree = do
          root <- allocTree tree
          new <- unsafeUseAsCString initStub $ \stub -> mkFunTransform (castPtrToFunPtr stub) fun root
          readTree new
        intS = sizeOf (undefined::Int) ; ptrS = sizeOf (undefined::Ptr())
        pok e p = poke (castPtr p) e >> return (p`plusPtr`sizeOf e)
        pik p = peek (castPtr p) >>= \e -> return (e,p`plusPtr`sizeOf e)
        allocTree (Group g) = do
          p <- mallocBytes (intS+intS+(length g*ptrS))
          p' <- pok (0::Int) p
          p'' <- pok (length g) p'
          mapM allocTree g >>= \l -> pokeArray (castPtr p'') l
          return p
        allocTree (Symbol (ID s)) = do
          p <- mallocBytes (intS+intS)
          p' <- pok (1::Int) p
          pok s p'
          return p
        readTree p = do
          (t,p') <- pik p
          case t :: Int of
            0 -> do
              (s,p'') <- pik p'
              l <- peekArray s p''
              liftM Group $ mapM readTree l
            1 -> do
              liftM (Symbol . ID) $ peek p'
              
