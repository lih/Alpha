{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ParallelListComp #-}
import Compile
import Context
import Data.ByteString.Unsafe
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Bimap as BM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Serialize as Ser
import Elf (writeElf)
import Foreign hiding (void)
import My.Control.Monad
import My.Control.Monad.State hiding ((<.>))
import My.Prelude
import Options
import PCode
import Serialize
import Specialize
import Syntax
import Syntax.Parse
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

version = "0.99"
printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++version

newtype Str = Str String
instance Show Str where show (Str s) = s

doTestOlder = return True -- for testing purposes, turn makefile-style file dependencies checks on or off

doCompile opts = case programs opts of
  [] -> interactive
  progs -> mapM_ compileProgram progs
  where
    languageFile language = languageDir opts</>language<.>"l"
    findSource language = findM doesFileExist (concat [[base<.>"a",base] | dir <- sourceDirs opts
                                                                         , let base = dir</>language])
    readProg s = let (a,':':b) = break (==':') s in (a,b)

    interactive = void $ compileFile "/dev/stdin"
    compileProgram (readProg -> (language,root)) = withDefaultContext $ do
      importLanguage compileLanguage (const $ return ()) language
      rootSym <- stateF languageF $ internSym root
      getAddressComp (outputArch opts) rootSym
      (addrs,ptrs) <- unzip $< sortBy (comparing fst) $< M.elems $< gets compAddresses
      top <- gets compTop
      contents <- B.concat $< sequence [withForeignPtr ptr $ \p -> unsafePackCStringLen (castPtr p,size)
                                       | ptr <- ptrs | size <- zipWith (-) (tail addrs++[top]) addrs]
      writeElf root contents
    compileLanguage name = debugM $ do
      source <- fromMaybe (error $ "Couldn't find source file for language "++name) $< findSource name
      let langFile = languageFile name
      b <- doTestOlder <&&> fileExist langFile <&&> (langFile `newerThan` source)
      if b then either error id $< Ser.decode $< B.readFile langFile else do
        putStrLn $ "Compiling language "++name
        lang <- compileFile source
        createDirectoryIfMissing True (dropFileName langFile)
        B.writeFile langFile (Ser.encode lang)
        return lang
    loadLanguage name = compileLanguage name >>= execCode . initializeL >> languageState get

    compileFile src = withDefaultContext $ (>> gets language) $ do
      str <- readFile src
      let sTree = concat $ parseAlpha src str
      code <- mapM compileExpr sTree
      languageState $ modify $ \e -> exportLanguage $ e { initializeL = foldr concatCode [] code }
      where compileExpr expr = do
              symExpr <- languageState $ envCast expr
              trExpr <- doTransform symExpr
              (code,imports) <- languageState $ compile Nothing trExpr
              mapM_ (importLanguage compileLanguage (execCode . initializeL)) imports
              execCode code
              return code

