{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Ex2 where

import Ex1  -- Para pegar o efeito Teletype

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Resource


data CustomException = ThisException | ThatException deriving Show
data AnotherException = AnotherException deriving Show

-- Usando o Teletype do Ex1
program :: Members '[Resource, Teletype, Error CustomException, Error AnotherException] r => Sem r ()
program = catch @CustomException work $ \e -> writeTTY ("Peguei " ++ show e)
  where
    work = bracket allocateResource (const $ writeTTY "Saindo do bracket") $ \input -> do
      writeTTY "Entrando no bracket"
      case input of
        "boom"     -> throw ThisException
        "tufe"     -> writeTTY (input <> "!!!") >> throw ThatException
        "shine"    -> writeTTY "F*deu!!!" >> throw AnotherException
        _          -> writeTTY ("Você disse: " <> input) >> writeTTY ">>> Nenhuma exceção! 🎉"
    allocateResource = do
      writeTTY "Vou alocar um recurso!"
      readTTY

runProgram :: IO (Either AnotherException (Either CustomException ()))
runProgram
  = (runM .@ lowerResource)
  . runError @AnotherException
  . runError @CustomException
  $ teletypeToIO program

main :: IO ()
main = do
  result <- runProgram
  putStrLn $ "Deu: " <> show result

{-
    runM :: Monad m => Sem '[Embed m] a -> m a

    lowerResource :: Member (Embed IO) r
                   => (forall x. Sem r x -> IO x)
                   -> Sem (Resource ': r) a
                   -> Sem r a

    (.@) :: Monad m
         => (forall x. Sem r x -> m x)
         -> (forall y. (forall x. Sem r x -> m x) -> Sem (e ': r) y -> Sem r y)
         -> Sem (e ': r) z
         -> m z

    runError :: Sem (Error e ': r) a -> Sem r (Either e a)
-}
