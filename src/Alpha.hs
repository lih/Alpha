{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ParallelListComp #-}
import System.Environment as SE
import System.FilePath
import System.Directory
import System.Posix.Files
import Foreign hiding (void)
import Data.Ord
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Serialize as Ser
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import Data.List
import My.Control.Monad
import My.Control.Monad.State hiding ((<.>))
import My.Prelude
import Options
import Elf (writeElf)
import Syntax
import Syntax.Parse
import Compile
import Serialize
import Specialize
import Context
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
  
version = "0.9.8"
printHelp = putStrLn helpMsg
printVersion = putStrLn $ "Alpha version "++version
  
newtype Str = Str String
instance Show Str where show (Str s) = s

doTestOlder = return True -- for testing purposes, turn makefile-style file dependencies on or off 

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
      writeElf language contents
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
      where compileExpr expr = print (fmap Str expr) >> do
              symExpr <- languageState $ envCast expr
              print symExpr
              trExpr <- doTransform symExpr
              (code,imports) <- languageState $ compile Nothing trExpr
              mapM_ (importLanguage compileLanguage (execCode . initializeL)) imports
              execCode code 
              return code
      
-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

