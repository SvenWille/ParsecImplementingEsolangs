{-# LANGUAGE ScopedTypeVariables #-}
module BrainFlip.Repl
    (repl
    ) where

import BrainFlip.Interpreter
import System.IO
import System.Console.Haskeline
import Control.Monad.Extra
import Control.Monad.Trans.Class
import System.Console.Haskeline.MonadException
import Data.Char

repl :: IO()
repl = putStrLn "Welcome to the BrainFlip-Interpreter (Version 0.1.0.0) :)" >> runInputT defaultSettings  replLoop



replLoop ::  InputT IO()
replLoop = do
  currentLine <- getInputLine prompt
  handleInput (fmap (filter (not . isSpace)) currentLine)
  where
    prompt :: String
    prompt = "BrainFlip: "

    handleInput :: Maybe String -> InputT IO ()
    handleInput currentLine =
      case currentLine of
        Just "quit"-> return ()
        Just "" -> replLoop
        Nothing -> return()
        Just x -> catch (lift ( executeTokens $ fst $ head $ parseBrainFlip x) >> lift (hFlush stdout) >> outputStrLn "" >> replLoop) (\(e::SomeException) -> outputStrLn "invalid input" >> replLoop )
