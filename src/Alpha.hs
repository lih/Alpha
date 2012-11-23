{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ParallelListComp, TupleSections #-}
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
import Elf (writeElf,entryAddress)
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

formatEntry Elf64 = entryAddress
formatEntry (Raw n) = n

version = "0.99999"
printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++version

newtype Str = Str String
instance Show Str where show (Str s) = s

doTestOlder = return True -- for testing purposes, turn makefile-style file dependencies checks on or off

doCompile opts = case programs opts of
  [] -> interactive
  progs -> mapM_ compileProgram progs
  where
    entry = formatEntry $ outputFmt opts
    languageFile language = languageDir opts</>language<.>"l"
    findSource language = findM doesFileExist (concat [[base<.>"a",base] | dir <- sourceDirs opts
                                                                         , let base = dir</>language])

    interactive = void $ compileFile "/dev/stdin"
    compileProgram (language,root) = withDefaultContext entry $ do
      importLanguage compileLanguage (const $ return ()) language
      l <- doF languageF get
      rootSym <- stateF languageF $ internSym root
      getAddressComp (outputArch opts) rootSym
      (addrs,ptrs) <- unzip $< sortBy (comparing fst) $< M.elems $< gets compAddresses
      top <- gets compTop
      contents <- B.concat $< sequence [withForeignPtr ptr $ \p -> unsafePackCStringLen (castPtr p,size)
                                       | ptr <- ptrs | size <- zipWith (-) (tail addrs++[top]) addrs]
      case outputFmt opts of
        Elf64 -> writeElf root contents
        Raw _ -> B.writeFile root contents
    compileLanguage force name = do
      let langFile = languageFile name
      source <- findSource name
      skip <- return (not force) <&&> fileExist langFile
              <&&> maybe (return True) (\s -> langFile `newerThan` s) source
      l <- if skip then either (\e -> error $ "Error reading language file "++langFile++": "++e)
                        id $< Ser.decode $< B.readFile langFile else do
             putStrLn $ "Compiling language "++name++"..."
             lang <- compileFile $ fromMaybe (error $ "Couldn't find source file for language "++name) source
             createDirectoryIfMissing True (dropFileName langFile)
             B.writeFile langFile (Ser.encode lang)
             return lang
      return (not skip,l)

    compileFile src = withDefaultContext entry $ (>> gets language) $ do
      str <- readFile src
      let sTree = concat $ parseAlpha src str
      init <- mapM compileExpr sTree
      languageState $ modify $ \e -> exportLanguage $ e { initializeL = init }
      where compileExpr expr = do
              symExpr <- languageState $ envCast expr
              trExpr <- doTransform symExpr
              (code,imports) <- languageState $ compile [] Nothing trExpr
              mapM_ (importLanguage compileLanguage (mapM_ execCode . initializeL)) imports
              execCode code
              return code

