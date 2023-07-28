module Brainfuck where

import Control.Monad.State as ST
import Data.Either
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

data Instr
  = JEZ
  | JNZ
  | INC
  | DEC
  | INCB
  | DECB
  deriving (Show, Eq)

data Jump
  = Jez Int Int
  | Jnz Int Int
  deriving (Show, Eq)

data Stmt
  = Inc Int
  | Dec Int
  | IncB Int
  | DecB Int
  deriving (Show, Eq)

toInstr :: Char -> Instr
toInstr '>' = INC
toInstr '<' = DEC
toInstr '[' = JEZ
toInstr ']' = JNZ
toInstr '+' = INCB
toInstr '-' = DECB

prog is = case parse p "" is of
  Right is -> is
  Left _ -> error "Noo"

instr :: Parser Instr
instr = toInstr <$> oneOf "><+-[]"

type RawProgram = [Instr]

type Program = [Either Jump Stmt]

p :: Parser [Instr]
p = spaces *> sepBy instr spaces <* spaces

toProgram :: RawProgram -> Program -- Should be a (Maybe Program) (unbalanced jumps)
toProgram is = fromRaw pairs <$> numbered 
  where
    numbered = zip [1 ..] is
    pairs = pairJumps $ filter (\i -> snd i `elem` [JNZ, JEZ]) numbered

fromRaw :: [(Int, Int)] -> (Int, Instr) -> Either Jump Stmt
fromRaw ps (n, JEZ)  = Left (Jez n (fromJust (lookup n ps)))
fromRaw ps (n, JNZ)  = Left (Jnz n (fromJust (lookup n ps)))
fromRaw ps (n, INC)  = Right $ Inc n
fromRaw ps (n, DEC)  = Right $ Dec n
fromRaw ps (n, INCB) = Right $ IncB n
fromRaw ps (n, DECB) = Right $ DecB n

pairJumps :: [(Int, Instr)] -> [(Int, Int)]
pairJumps js = concatMap (\((i, _), (i', _)) -> [(i, i'), (i', i)]) (go js [])
  where
    go [] s = []
    go (j : js) [] = go js [j]
    go (j : js) s@(j' : js') = if snd j == snd j' then go js (j : s) else (j, j') : go js js'

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

evalJump :: Jump -> VM -> VM
evalJump (Jez _ n) s = if currentByte s == 0 then s {ip = n} else s
evalJump (Jnz _ n) s = if currentByte s /= 0 then s {ip = n} else s

evalStmt :: Stmt -> VM -> VM
evalStmt (Inc _) s = s {dp = succ (dp s)}
evalStmt (Dec _) s = s {dp = pred (dp s)}
evalStmt (IncB _) s = s {memory = incMemory (memory s) (dp s)}
evalStmt (DecB _) s = s {memory = decMemory (memory s) (dp s)}

eval :: Program -> ST.State VM ()
eval is = do
  m <- get
  if length is <= ip (trace (show m) m)
    then return ()
    else do
      let i = is !! ip m
      case i of
        Left  j -> do
          put $ incIp (evalJump j m)
          eval is
        Right s -> do
          put $ incIp (evalStmt s m)
          eval is

initState = VM {dp = 0, ip = 0, memory = replicate 10 0}

run :: String -> VM
run is = execState (eval prog) initState
  where
    prog = case parse p "" is of
      Right is -> toProgram is
      Left _ -> error "Noo"
