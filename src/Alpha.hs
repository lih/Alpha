import System.Environment as SE
import System.FilePath
import System.Directory
import System.Posix.Files
import Alpha.Options
import Alpha.Elf (writeElf)
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
import Util.Monad
import Control.Monad.State
import Util.Prelude
-- import Specialize
import IR

main = do
  args <- getArgs
  case getSettings args of
    Right s -> execute s
    Left err -> fail err

execute s = case action s of
  PrintHelp -> printHelp
  PrintVersion -> printVersion
  Compile b -> print s >> doCompile b s
  
printHelp = putStrLn helpMsg
printVersion = putStrLn "Alpha version 1.0"
  
newtype Str = Str String
instance Show Str where show (Str s) = s

doCompile b s = if b then void $ compileFile "/dev/stdin" (runes s)
                else mapM_ compileRune (runes s)
  where 
    runeFile rune = runeDir s</>rune<.>"r"
    findScroll rune = findM fileExist (concat [[base,base<.>"a"] | dir <- scrollDirs s, let base = dir</>rune])
    compileFile src runes = withEnv defaultEnv $ (>>E.getEnv) $ do 
      str <- readFile src
      let sTree = concat $ parseAlpha src str
      mapM_ doImport runes
      code <- mapM compileExpr sTree
      stateEnv $ modify (\e -> e { load = foldr concatCode [] code })
    compileRune rune = do
      src <- fromMaybe (error$"Couldn't find source file for rune "++rune) $< findScroll rune
      let obj = runeFile rune
      ifM (andM [fileExist $ runeFile rune,liftM2 (<) (getFileModTime src) (getFileModTime obj)]) 
          (putStrLn $ "Rune "++rune++" already compiled. Skipping.") $ do 
        env <- compileFile src []
        print env
        createDirectoryIfMissing True (dropFileName obj)
        B.writeFile obj (encode $ exportContext env)
    compileExpr expr = do
      (expr',env) <- envCast expr $< E.getEnv
      (code,env') <- compile env Nothing $< doTransform expr'
      print expr
      setEnv (context env')
      mapM_ doImport (imports env')
      return code
    doImport im = do 
      e <- E.getEnv
      putStrLn $ "Importing rune "++im
      unless (isImport im e) $ do 
        compileRune im
        let rune = runeFile im
        ce <- either error id $< decode $< B.readFile rune
        print ce
        mapM_ doImport (getImports ce)
        e <- E.getEnv ; E.setEnv $ merge e (im,ce)
      
-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

