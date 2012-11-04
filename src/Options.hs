module Options (Architecture(..),Action(..),Settings(..),helpMsg,getSettings) where

import Specialize.Architecture
import System.Console.GetOpt
import Data.Maybe
import My.Data.List

data Flag = Help | Version
          | SourceDir FilePath 
          | Program FilePath 
          | LanguageDir FilePath 
          | Architecture Architecture 
          deriving Show
data Action = PrintHelp | PrintVersion | Compile
            deriving Show

data Settings = Settings {
  action      :: Action,
  sourceDirs  :: [FilePath],
  languageDir :: FilePath,
  programs    :: [FilePath],
  outputArch  :: Architecture
  }
              deriving Show

options = 
  [Option ['h'] ["help"] (NoArg Help) 
   "prints usage information"
  ,Option ['v'] ["version"] (NoArg Version) 
   "prints Alpha's version information"
  ,sep
  ,Option ['S'] ["source-dir"] (ReqArg SourceDir "DIR") 
   "adds DIR to the list of directories searched for sources"
  ,Option ['L'] ["language-dir"] (ReqArg LanguageDir "DIR") 
   "casts all languages in DIR (default '.')"
  ,sep
  ,Option ['a'] ["architecture"] (ReqArg (Architecture . str2arch) "ARCH") 
   $ "specializes for ARCH instead of the local architecture (ARCH is one of "++foldr glue "" (tails archNames)++")"
  ]
  where str2arch s = fromMaybe (error $ "Invalid architecture name "++s) $ lookup s $ zip archNames architectures
        archNames = map archName architectures
        glue [a] _ = a
        glue [a,_] t = a++" or "++t
        glue (a:_) t = a++", "++t
        sep = Option [] [] undefined "-----------------"
helpMsg = usageInfo "Usage: Alpha <options> <files>" options

defaultSettings progs = Settings Compile ["."] "." progs hostArch
getSettings [] = Right $ defaultSettings []
getSettings args = case getOpt Permute options args of
  (opts,mods,[]) -> Right $ foldl handleOpt (defaultSettings mods) opts
  (_,_,err) -> Left $ helpMsg ++ concatMap ("\n"++) err
  where handleOpt s Help             = s { action = PrintHelp }
        handleOpt s Version          = s { action = PrintVersion }
        handleOpt s (SourceDir d)    = s { sourceDirs = d : sourceDirs s }
        handleOpt s (LanguageDir d)  = s { languageDir = d }
        handleOpt s (Architecture a) = s { outputArch = a }

