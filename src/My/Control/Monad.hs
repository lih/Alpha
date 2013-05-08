{-# LANGUAGE NoMonomorphismRestriction #-}
-- |A module defining some additional utilities from Control.Monad
module My.Control.Monad (
  module Control.Monad, module Control.Applicative,
  -- * The flipped equivalents to the @$@ and @<$>@ operators
  (§),(<§>),
   -- * Returning the first argument
   prog1,(->>=),
   -- * Boolean monadic operators
   (<&&>),(<||>),
   -- * If and find with boolean monadic arguments
   ifM,whenM,findM,
   -- * Misc
   doNothing,pairM) where

import Control.Monad
import Control.Applicative

-- |Flipped equivalent to '$' (@f $ x@ is equivalent to @x § f@) 
(§) :: a -> (a -> b) -> b
(§) = flip ($) 
-- |Flipped equivalent to '<$>'
(<§>) :: Applicative f => f a -> (a -> b) -> f b
(<§>) = flip (<$>)

-- |The Haskell equivalent to Lisp's @prog1@. @prog1 a b@ executes @a@ and @b@
-- in order (as with '>>=') then returns the value of @a@
prog1 a b = a >>= \x -> b x >> return x
-- |Infix equivalent to 'prog1'
(->>=) = prog1

-- |Monadic '&&' with short-circuit evaluation (left-biased)
a <&&> b = ifM a b (return False)
-- |Monadic '||' with short-circuit evaluation (left-biased)
a <||> b = ifM a (return True) b
infixr 3 <&&>
infixr 3 <||>

-- |Monadic 'if' function
ifM b th el = b >>= \b' -> if b' then th else el
-- |'when' with monadic condition
whenM b m = ifM b m (return ())
-- |Monadic 'find' function
findM p l = foldr fun (return Nothing) l
  where fun x ret = ifM (p x) (return $ Just x) ret

-- |The name says it all
doNothing = return ()
-- |Constructs a pair from two monadic values
pairM = liftM2 (,)
