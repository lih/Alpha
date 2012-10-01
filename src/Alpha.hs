import System.Environment as SE
import System.FilePath
import System.Directory
import System.Posix.Files
import Options
import Elf (writeElf)
import Syntax
import Syntax.Parse
import Environment.Foreign as E
import Environment.Context
import Compile
import Compile.State
import Serialize
import Data.Serialize
import qualified Data.ByteString as B
import Data.Maybe
import My.Control.Monad
import Control.Monad.State
import My.Prelude
import Specialize
import PCode

main = do
  args <- getArgs
  case getSettings args of
    Right s -> execute s
    Left err -> fail err

execute s = case action s of
  PrintHelp -> printHelp
  PrintVersion -> printVersion
  Compile -> print s >> doCompile s
  
version = "0.9.0.1"
printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++version
  
newtype Str = Str String
instance Show Str where show (Str s) = s

doTestOlder = return False

doCompile opts = case programs opts of 
  [] -> interactive
  progs -> mapM_ compileProgram progs
  where   
    languageFile language = languageDir opts</>language<.>"l"
    findSource language = findM fileExist (concat [[base,base<.>"a"] | dir <- sourceDirs opts
                                                                     , let base = dir</>language])
    interactive = void $ compileFile "/dev/stdin"     
    compileProgram prog = error "unimplemented"
    compileLanguage name = do
      source <- fromMaybe (error$"Couldn't find source file for language "++name) $< findSource name
      let language = languageFile name
      ifM (doTestOlder <&&> fileExist language <&&> (language `newerThan` source)) 
          (putStrLn $ "Language "++name++" already compiled. Skipping.") $ do 
        env <- compileFile source
        print env
        createDirectoryIfMissing True (dropFileName language)
        B.writeFile language (encode $ exportContext env)
    
    doImport imp = stateEnvT $ addImport getImp imp
      where getImp imp = do
              putStrLn $ "Importing language "++imp
              compileLanguage imp
              let language = languageFile imp
              ce <- either error id $< decode $< B.readFile language
              print ce
              return ce
        
    compileFile src = withEnv defaultEnv $ (>>E.getEnv) $ do 
      str <- readFile src
      let sTree = concat $ parseAlpha src str
      code <- mapM compileExpr sTree
      stateEnv $ modify (\e -> e { loadCode = foldr concatCode [] code })
      where compileExpr expr = print expr >> do
              symExpr <- stateEnv $ envCast expr
              trExpr <- doTransform symExpr
              (code,cs) <- stateEnv $ compile Nothing trExpr
              mapM_ doImport (imports cs)
              return code
      
-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

