module Brainfuck where

import Control.Monad.State as ST
import Data.Either
import Debug.Trace
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
incIp s = s {ip = succ (ip s)}

currentByte :: VM -> Int
currentByte s = memory s !! dp s

evalStep :: Stmt -> VM -> Program -> VM
evalStep INC s _ = s {dp = succ (dp s)}
evalStep DEC s _ = s {dp = pred (dp s)}
evalStep INCB s _ = s {memory = incMemory (memory s) (dp s)}
evalStep DECB s _ = s {memory = decMemory (memory s) (dp s)}
evalStep JEZ s p = if currentByte s == 0 then s { ip = jumpForward p (ip s) } else s
evalStep JNZ s p = if currentByte s /= 0 then s { ip = jumpBackward p (ip s) } else s

jumpBackward :: Program -> Int -> Int
jumpBackward p i = evalState (go p (i - 1)) 0
  where
    go p i = do
      case p !! i of
        JEZ -> do
          n <- get
          if n == 0 then return i else put (n - 1) >> go p (i - 1)
        JNZ -> do
          n <- get
          put (n + 1)
          go p (i - 1)
        _ -> go p (i - 1)

jumpForward :: Program -> Int -> Int
jumpForward p i = evalState (go p (i + 1)) 0
  where
    go p i = do
      case p !! i of
        JNZ -> do
          n <- get
          if n == 0 then return i else put (n - 1) >> go p (i + 1)
        JEZ -> do
          n <- get
          put (n + 1)
          go p (i + 1)
        _ -> go p (i + 1)

eval :: Program -> ST.State VM ()
eval is = do
  m <- get
  if length is <= ip (trace (show m) m)
    then return ()
    else do
      let i = is !! ip m
      put $ incIp (evalStep i m is)
      eval is

initState = VM {dp = 0, ip = 0, memory = replicate 10 0}

run :: String -> VM
run is = execState (eval prog) initState
  where prog = case parse p "" is of
                  Right is -> is
                  Left _ -> error "Noo"
  
