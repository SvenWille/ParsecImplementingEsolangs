module BrainFlip.Interpreter
    ( parseBrainFlip , executeTokens, interpreteFile
    ) where

import Prelude hiding (repeat)
import Text.ParserCombinators.ReadP
import Data.Word
import Data.Stream
import Control.Monad
import Data.Char
import Control.Applicative
import System.IO
import Control.Monad.Loops


data Token = INN | OUT | SHL | SHR | INC | DEC | LOOP [Token] | BINDING [Token] | SET Word8

data BrainFlipBand = Band { curr :: Word8 , lft :: Stream Word8 , rgt :: Stream Word8}

type LibFiles = [FilePath]

newBand :: BrainFlipBand
newBand = Band { curr = 0 , lft  = repeat 0 , rgt = repeat  0}

parseBrainFlip :: ReadS [Token]
parseBrainFlip =  readP_to_S tokens

tokens :: ReadP [Token]
tokens  = manyTill (atomicToken <++ loop <++ set  ) eof

set :: ReadP Token
set = do
  string "set"
  digits <- between (char '(') (char ')') (munch1 isDigit)
  return (SET $ read digits)

loop :: ReadP Token
loop = between (char '[') (char ']') (fmap LOOP tokensLoop)
  where
    tokensLoop :: ReadP [Token]
    tokensLoop  = many1 (loop <++ atomicToken )


atomicToken :: ReadP Token
atomicToken = inn <++ out  <++ shl <++ shr <++ inc <++ dec
  where
    inn,out,shl,shr,inc,dec :: ReadP Token
    inn = char ',' >> return INN
    out = char '.' >> return OUT
    shl = char '<' >> return SHL
    shr = char '>' >> return SHR
    inc = char '+' >> return INC
    dec = char '-' >> return DEC

{-
binding :: ReadP Token
binding = do
  string "let"
  skipMany1 (char ' ')
  ident <- munch1
  skipMany1 (chat ' ')
  where
    ident :: ReadP String
    ident = optional  <++
  -}




executeTokens :: [Token] -> IO()
executeTokens = foldM_ singleStep newBand

executeLoop :: BrainFlipBand -> [Token] -> IO BrainFlipBand
executeLoop = foldM singleStep

singleStep :: BrainFlipBand -> Token -> IO BrainFlipBand
singleStep old@(Band cur lf rt) tok =
  case tok of
    INN -> hSetBuffering stdin NoBuffering >> getChar >>= \x -> hSetBuffering stdin LineBuffering >> return (Band (fromIntegral $ fromEnum x) lf rt)
    OUT -> putChar (chr $ fromIntegral cur) >> return old
    SHL -> return (Band (Data.Stream.head lf) (Data.Stream.tail lf) (Cons cur rt) )
    SHR -> return (Band (Data.Stream.head rt) (Cons cur lf) (Data.Stream.tail rt) )
    INC -> return (Band (cur + 1) lf rt)
    DEC -> return (Band (cur -1) lf rt)
    LOOP tkns ->  iterateUntilM ( (== 0) . curr ) (`executeLoop`  tkns) old
    SET value -> return (old{curr = value})
    _ -> error "apparently a command or multiple commands are not yet implemented"



interpreteFile :: FilePath -> LibFiles -> IO()
interpreteFile f libs = do
  cont <- readFile f
  --loadLibs
  executeTokens $ fst $ Prelude.head $ parseBrainFlip (Prelude.filter (not . isSpace) cont)

--loadLibs :: LibFiles -> IO
--loadLibs =
