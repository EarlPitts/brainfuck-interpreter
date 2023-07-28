module Brainfuck where

import Data.Word
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
  = Jez Int
  | Jnz Int
  deriving (Show, Eq)

data Stmt
  = Inc
  | Dec
  | IncB
  | DecB
  deriving (Show, Eq)

toInstr :: Char -> Instr
toInstr '>' = INC
toInstr '<' = DEC
toInstr '[' = JEZ
toInstr ']' = JNZ
toInstr '+' = INCB
toInstr '-' = DECB

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
fromRaw ps (n, JEZ) = Left (Jez (fromJust (lookup n ps)))
fromRaw ps (n, JNZ) = Left (Jnz (fromJust (lookup n ps)))
fromRaw ps (n, INC) = Right Inc
fromRaw ps (n, DEC) = Right Dec
fromRaw ps (n, INCB) = Right IncB
fromRaw ps (n, DECB) = Right DecB

-- Finds each pair of jumps and pairs them together
pairJumps :: [(Int, Instr)] -> [(Int, Int)]
pairJumps js = concatMap (\((i, _), (i', _)) -> [(i, i'), (i', i)]) (go js [])
  where
    go [] s = []
    go (j : js) [] = go js [j]
    go (j : js) s@(j' : js') = if snd j == snd j' then go js (j : s) else (j, j') : go js js'

data VM = VM
  { dp :: Int,
    ip :: Int,
    memory :: [Word8]
  }
  deriving (Show, Eq)

incMemory :: [Word8] -> Int -> [Word8]
incMemory m i = take i m ++ [succ (m !! i)] ++ drop (i + 1) m

decMemory :: [Word8] -> Int -> [Word8]
decMemory m i = take i m ++ [pred (m !! i)] ++ drop (i + 1) m

incIp :: VM -> VM
incIp s = s {ip = succ (ip s)}

currentByte :: VM -> Word8
currentByte s = memory s !! dp s

evalJump :: Jump -> VM -> VM
evalJump (Jez n) s = if currentByte s == 0 then s {ip = n} else s
evalJump (Jnz n) s = if currentByte s /= 0 then s {ip = n} else s

evalStmt :: Stmt -> VM -> VM
evalStmt Inc s = s {dp = succ (dp s)}
evalStmt Dec s = s {dp = pred (dp s)}
evalStmt IncB s = s {memory = incMemory (memory s) (dp s)}
evalStmt DecB s = s {memory = decMemory (memory s) (dp s)}

eval :: Program -> ST.StateT VM IO ()
eval is = do
  m <- get
  if length is <= ip m
    then return ()
    else do
      let i = is !! ip m
      liftIO $ print m
      case i of
        Left j -> do
          put $ incIp (evalJump j m)
          eval is
        Right s -> do
          put $ incIp (evalStmt s m)
          eval is

initState = VM {dp = 0, ip = 0, memory = replicate 10 0}

run :: String -> IO VM
run is = execStateT (eval prog) initState
  where
    prog = case parse p "" is of
      Right is -> toProgram is
      Left _ -> error "Noo"
