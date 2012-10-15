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

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

