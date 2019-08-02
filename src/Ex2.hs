{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Ex2 where

import Ex1

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Resource

-- Using Teletype effect from above

data CustomException = ThisException | ThatException deriving Show

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch @CustomException work $ \e -> writeTTY ("Caught " ++ show e)
  where work = bracket (readTTY) (const $ writeTTY "exiting bracket") $ \input -> do
          writeTTY "entering bracket"
          case input of
            "explode"     -> throw ThisException
            "weird stuff" -> writeTTY input >> throw ThatException
            _             -> writeTTY input >> writeTTY "no exceptions"

main :: IO (Either CustomException ())
main = (runM .@ lowerResource .@@ lowerError @CustomException) . teletypeToIO $ program
