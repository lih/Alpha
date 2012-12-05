{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ParallelListComp, TupleSections #-}
import Compile
import Context
import Data.ByteString.Unsafe
import Data.List
import Data.Maybe
import Data.Ord
import Data.Version
import qualified Data.Bimap as BM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Serialize as Ser
import Format
import Foreign hiding (void)
import My.Control.Monad
import My.Control.Monad.State hiding ((<.>))
import My.Prelude
import Options
import Paths_alpha (version)
import PCode
import Serialize
import Specialize
import Syntax
import Syntax.Parse
import System.IO
import System.Directory
import System.Environment as SE
import System.FilePath
import System.Posix.Files

main = do
  args <- getArgs
  case getSettings args of
    Right s -> execute s
    Left err -> fail err

execute s = case action s of
  PrintHelp -> printHelp
  PrintVersion -> printVersion
  Compile -> doCompile s

printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++showVersion version

newtype Str = Str String
instance Show Str where show (Str s) = s

doCompile opts = case programs opts of
  [] -> interactive
  progs -> mapM_ compileProgram progs
  where
    entry = formatEntry $ outputFmt opts
    languageFile language = languageDir opts</>language<.>"l"
    findSource language = findM doesFileExist [file | dir <- sourceDirs opts
                                                    , let base = dir</>language
                                                    , file <- [base<.>"a",base]]

    interactive = withDefaultContext entry $ do
      putStrLn $ "Alpha, version "++showVersion version++". Type alpha/help() for help on Alpha invocation. ^D to exit."
      str <- getContents
      let sTree = concat $ parseAlpha "/dev/stdin" str
      mapM_ (\e -> compileExpr e >> putStr "> " >> hFlush stdout) (Group []:sTree)
      putStrLn "\rGoodbye !"
    compileProgram (language,entryName) = withDefaultContext entry $ do
      importLanguage compileLanguage (const $ return ()) language
      l <- getting language_
      entrySym <- viewState language_ $ internSym entryName
      _ <- getAddressComp (outputArch opts) entrySym
      (addrs,ptrs) <- unzip $< sortBy (comparing fst) $< M.elems $< gets compAddresses
      top <- gets compTop
      contents <- B.concat $< sequence [withForeignPtr ptr $ \p -> unsafePackCStringLen (castPtr p,size)
                                       | ptr <- ptrs | size <- zipWith (-) (tail addrs++[top]) addrs]
      writeFormat (outputFmt opts) entryName contents
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
        where compileFile src = withDefaultContext entry $ (>> gets language) $ do
                str <- readFile src
                let sTree = concat $ parseAlpha src str
                init <- mapM compileExpr sTree
                languageState $ modify $ \e -> exportLanguage $ e { initializeL = init }
    compileExpr expr = do
      symExpr <- languageState $ envCast expr
      trExpr <- doTransform symExpr
      (code,imports) <- languageState $ compile [] Nothing trExpr
      mapM_ (importLanguage compileLanguage (mapM_ execCode . initializeL)) imports
      execCode code
      return code

