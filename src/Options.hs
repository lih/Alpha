module Options (Architecture(..),Action(..),Settings(..),Format(..),helpMsg,getSettings) where

import Specialize.Architecture
import System.Console.GetOpt
import Data.Maybe
import My.Data.List

data Flag = Help | Version
          | SourceDir FilePath 
          | Program FilePath 
          | LanguageDir FilePath 
          | Architecture Architecture
          | Format Format
          deriving Show
data Action = PrintHelp | PrintVersion | Compile
            deriving Show
data Format = Elf64 | Raw Int
            deriving Show
data Settings = Settings {
  action      :: Action,
  sourceDirs  :: [FilePath],
  languageDir :: FilePath,
  programs    :: [(String,String)],
  outputArch  :: Architecture,
  outputFmt   :: Format
  }
              deriving Show

splitArg s = case break (==':') s of
  (a,':':b) -> a:splitArg b
  ("","") -> []
  (a,"") -> [a]

options = 
  [Option ['h','?'] ["help"] (NoArg Help) 
   "prints usage information"
  ,Option ['v'] ["version"] (NoArg Version) 
   "prints Alpha's version information"
  ,sep
  ,Option ['S'] ["source-dir"] (ReqArg SourceDir "<dir>") 
   "adds <dir> to the list of directories searched for source files"
  ,Option ['L'] ["language-dir"] (ReqArg LanguageDir "<dir>") 
   "writes and seeks all language files in <dir> (defaults to the current directory)"
  ,sep
  ,Option ['a'] ["architecture"] (ReqArg (Architecture . str2arch) "<arch>") 
   $ "specializes for <arch> instead of the host architecture (<arch> can be one of "++foldr glue "" (tails archNames)++")"
  ,Option ['f'] ["format"] (ReqArg (Format . str2fmt) "<fmt>")
   $ "writes the output programs in the specified format (<fmt> can be one of elf64 or \n"
   ++"raw:<n> where <n> is the start address)"
  ]
  where str2arch s = fromMaybe (error $ "Invalid architecture name "++s) $ lookup s $ zip archNames architectures
        str2fmt s = case splitArg s of
          ["elf64"] -> Elf64
          ["raw",n] -> Raw (read n)
          _ -> error ("Invalid format argument "++show s)
        archNames = map archName architectures
        glue [a] _ = a
        glue [a,_] t = a++" or "++t
        glue (a:_) t = a++", "++t
        sep = Option [] [] undefined "-----------------"
helpMsg = usageInfo "Usage: alpha (<option>|<language>:<symbol>)..." options

defaultSettings progs = Settings Compile ["."] "." (map readProg progs) hostArch Elf64
  where readProg p = case splitArg p of
          [l,s] -> (l,s)
          _ -> error $ "Ill-formed argument '"++p++"'. Should be of the form <language>:<symbol>"
getSettings [] = Right $ defaultSettings []
getSettings args = case getOpt Permute options args of
  (opts,progs,[]) -> Right $ foldl handleOpt (defaultSettings progs) opts
  (_,_,err) -> Left $ helpMsg ++ concatMap ("\n"++) err
  where handleOpt s Help             = s { action = PrintHelp }
        handleOpt s Version          = s { action = PrintVersion }
        handleOpt s (SourceDir d)    = s { sourceDirs = d : sourceDirs s }
        handleOpt s (LanguageDir d)  = s { languageDir = d }
        handleOpt s (Architecture a) = s { outputArch = a }
        handleOpt s (Format f)       = s { outputFmt = f }

