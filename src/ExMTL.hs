module ExMTL where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Identity

data TooBigError = TooBigError deriving Show

-- ExceptT e (StateT s m) a

ex1 :: Monad m => Int -> ExceptT TooBigError (StateT String m) ()
ex1 x = do
  lift $ put (show x)
  if x > 5
    then throwE TooBigError
    else pure ()
  lift (put "99")

ex2 :: Monad m => Int -> StateT String (ExceptT TooBigError m) ()
ex2 x = do
  put (show x)
  if x > 5
    then lift $ throwE TooBigError
    else pure ()
  put "99"

ex1'
  = runIdentity
  . flip runStateT ""
  . runExceptT
  $ ex1 0

ex2'
  = runIdentity
  . runExceptT
  . flip runStateT ""
  $ ex2 0
