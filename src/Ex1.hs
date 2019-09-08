{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Ex1 where

import Polysemy
import Polysemy.Input
import Polysemy.Output

-- Seu efeito
data Teletype m a where
  -- Construtores = operações dentro do efeito
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

-- Magia
makeSem ''Teletype

-- Como interpretar "para valer"
teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

-- Preparando para interpretar nosso efeito em termos de outros existentes (puro)
runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  = runOutputMonoid pure   -- Para cada WriteTTY, pega a saída do programa e anexa a uma lista em uma ([String], a)
  . runInputList i         -- Cada elemento da lista é uma linha de input
  . reinterpret2 \case     -- Reinterpretar nosso efeito em termos de Input e Output
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg

-- Nosso programa. Só descreve as ações!
echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> writeTTY "Falou!!" >> pure ()
    _  -> writeTTY ("echo: " <> i) >> echo

-- Para testes
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

-- Só o que o nosso programa imprimiria na tela
pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

-- echo forever
main :: IO ()
main = runM . teletypeToIO $ echo
