module Brainfuck where

import Debug.Trace

import Control.Monad.State as ST
import Data.Either
import Text.Parsec
import Text.Parsec.String

data Stmt
  = JEZ
  | JNZ
  | INC
  | DEC
  | INCB
  | DECB
  deriving (Show, Eq)

toStmt :: Char -> Stmt
toStmt '>' = INC
toStmt '<' = DEC
toStmt '[' = JEZ
toStmt ']' = JNZ
toStmt '+' = INCB
toStmt '-' = DECB

stmt :: Parser Stmt
stmt = toStmt <$> oneOf "><+-[]"

p :: Parser [Stmt]
p = spaces *> sepBy stmt spaces <* spaces

type Program = [Stmt]

data VM = VM
  { dp :: Int,
    ip :: Int,
    memory :: [Int]
  }
  deriving (Show, Eq)

incMemory :: [Int] -> Int -> [Int]
incMemory m i = take i m ++ [succ (m !! i)] ++ drop (i + 1) m

decMemory :: [Int] -> Int -> [Int]
decMemory m i = take i m ++ [pred (m !! i)] ++ drop (i + 1) m

incIp :: VM -> VM
incIp s = s { ip = succ (ip s) }

currentByte :: VM -> Int
currentByte s = memory s !! dp s

evalStep :: Stmt -> VM -> VM
evalStep INC s   = incIp $ s {dp = succ (dp s)}
evalStep DEC s   = incIp $ s {dp = pred (dp s), ip = succ (ip s)}
evalStep INCB s  = incIp $ s {memory = incMemory (memory s) (dp s)}
evalStep DECB s  = incIp $ s {memory = decMemory (memory s) (dp s)}
-- evalStep JEZ s p  = if currentByte s == 0 then jumpForward p (ip s) else s
-- evalStep JNZ s p  = if currentByte s /= 0 then jumpBackward p (ip s) else s

jumpBack :: Program -> Int -> Int
jumpBack p i = undefined

jumpForward :: Program -> Int -> Int
jumpForward p i = undefined

eval :: Program -> ST.State VM ()
eval is = do
  m <- get
  if length is <= ip m
    then return ()
    else do
      let i = is !! ip m
      -- if isJump i
      -- then do
      -- else
      let newState = evalStep i m
      put $ newState { ip = succ (ip newState) }
      eval is

initState = VM {dp = 0, ip = 0, memory = replicate 10 0}

p' = case parse p "" "->+<" of
  Right is -> is
  Left _ -> error "Noo"
