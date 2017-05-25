{-# LANGUAGE PatternGuards #-}
module Main where

import Text.ParserCombinators.ReadP
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import BrainFlip.Repl
import BrainFlip.Interpreter
import Control.Monad.Extra

data Ops = Ops {
  startRepl :: Bool, --start the repl ?
  --printValue :: Bool --instead of printing the value on the band as characters, the integer value will be printed
  --keepReplBand :: Bool --if this flag is true the repl will keep its band after calculation
  help :: Bool
}


defaultOps :: Ops
defaultOps = Ops {startRepl = False , help = False}


options :: [OptDescr (Ops -> Ops)]
options  =  [Option ['r'] ["repl"] (NoArg (\opts -> opts{startRepl = True})) "start a interactive session",
             Option ['h'] ["help"] (NoArg (\opts -> opts{help = True})) "display help"]

usageString :: String
usageString = "Usage: BrainFun [Option ...] InputFiles "


main :: IO ()
main = parseOps >>= ifM (help . fst) (putStrLn . const ( usageInfo usageString options))  (uncurry replOrFile)


parseOps :: IO (Ops,[String])
parseOps = do
  args <- getArgs
  case getOpt Permute options args of
      (op,f,[]) -> return (foldl (\d opt -> opt d) defaultOps op ,f)
      (_,_,errs) ->  ioError (userError (concat errs ++ usageInfo usageString options))


replOrFile :: Ops -> [String]  -> IO()
replOrFile ops fs
  | [] <- fs      = repl
  | startRepl ops = repl
  | otherwise     = interpreteFile (head fs) (tail fs)
