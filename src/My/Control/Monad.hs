{-# LANGUAGE NoMonomorphismRestriction #-}
module My.Control.Monad (module Control.Monad,($<),(>$),(>$<),(§),(§<),(>§),(>§<),(<&&>),(<||>),ifM,findM,lift2) where

import Control.Monad
import Control.Monad.Trans

($<)  :: Monad m => (a -> b) -> m a -> m b
(>$)  :: Monad m => m (a -> b) -> a -> m b
(>$<) :: Monad m => m (a -> b) -> m a -> m b
($<) = liftM
mf >$ x = mf >>= \f -> return (f x)
(>$<) = ap

lift2 = lift . lift

infixr 0 $<,>$,>$<

(§) = flip ($)
(§<) = flip (>$)
(>§) = flip ($<)
(>§<) = flip (>$<)

ifM b th el = b >>= \b -> if b then th else el
a <&&> b = ifM a b (return False)
a <||> b = ifM a (return True) b
infixr 3 <&&>
infixr 3 <||>

findM p l = foldr fun (return Nothing) l
  where fun x ret = ifM (p x) (return $ Just x) ret

