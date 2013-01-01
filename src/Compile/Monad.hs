{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, NoMonomorphismRestriction #-}
module Compile.Monad where

import My.Control.Monad.State
import My.Control.Monad.WriterAcc
import ID
import PCode.Instruction
import Data.Map

data CompileState = CS {
  infoStack :: [(Int,[Int],Int,Maybe ID)],
  locals    :: Map ID ID
  }
infoStack_ = View (infoStack,\s cs -> cs { infoStack = s })
locals_ = View (locals,\s cs -> cs { locals = s })

newtype CompileT m a = CM { runCM :: StateT CompileState (WriterAccT [Instruction] (Sum Int) m) a }
                       deriving (Monad,MonadFix,MonadState CompileState,MonadWriter [Instruction])
instance MonadTrans CompileT where
  lift = CM . lift . lift

getPos = CM (liftM getSum (lift getAcc))
runCompileT (CM m) locals = runWriterAccT (evalStateT m (CS [] locals))

pushInfo        = modifying infoStack_ . (:)
popInfo         = viewState infoStack_ (\l -> case l of
                                           (h:t) -> (h,t)
                                           [] -> error "Invalid use of Axioms <- or -> outside of a choose expression")
topInfo         = getting (infoStack_ >>> f_ head)
withInfo f      = popInfo >>= \i -> f i >>= \ret -> pushInfo i >> return ret

