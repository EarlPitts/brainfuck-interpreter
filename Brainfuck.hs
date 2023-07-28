module Brainfuck where

import Control.Monad.State as ST
import Data.Either
import Data.List
import Data.ByteString.Internal
import Data.Maybe
import Data.Word
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import System.IO
import System.Environment

data Token
  = JEZ
  | JNZ
  | INC
  | DEC
  | INCB
  | DECB
  | OUT
  | IN
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

data IOStmt
  = In
  | Out
  deriving (Show, Eq)

data Instr
  = IOStmt IOStmt
  | Jump Jump
  | Stmt Stmt
  deriving (Show, Eq)

toToken :: Char -> Token
toToken '>' = INC
toToken '<' = DEC
toToken '[' = JEZ
toToken ']' = JNZ
toToken '+' = INCB
toToken '-' = DECB
toToken '.' = OUT
toToken ',' = IN

instr :: Parser Token
instr = toToken <$> oneOf "><+-[],."

type RawProgram = [Token]

type Program = [Instr]

p :: Parser [Token]
p = spaces *> sepEndBy instr spaces <* spaces

toProgram :: RawProgram -> Program -- Should be a (Maybe Program) (unbalanced jumps)
toProgram is = fromRaw pairs <$> numbered
  where
    numbered = zip [1 ..] is
    pairs = pairJumps $ filter (\i -> snd i `elem` [JNZ, JEZ]) numbered

fromRaw :: [(Int, Int)] -> (Int, Token) -> Instr
fromRaw ps (n, JEZ)  = Jump (Jez (fromJust (lookup n ps)))
fromRaw ps (n, JNZ)  = Jump (Jnz (fromJust (lookup n ps)))
fromRaw ps (n, INC)  = Stmt Inc
fromRaw ps (n, DEC)  = Stmt Dec
fromRaw ps (n, INCB) = Stmt IncB
fromRaw ps (n, DECB) = Stmt DecB
fromRaw ps (n, IN)   = IOStmt In
fromRaw ps (n, OUT)  = IOStmt Out

-- Finds each pair of jumps and pairs them together
pairJumps :: [(Int, Token)] -> [(Int, Int)]
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

modMemory :: [Word8] -> Int -> Word8 -> [Word8]
modMemory m i n = take i m ++ [n] ++ drop (i + 1) m

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

evalIOStmt :: IOStmt -> VM -> IO VM
evalIOStmt In s = do
  input <- getChar
  let n = c2w input 
  return $ s {memory = modMemory (memory s) (dp s) n}
evalIOStmt Out s = do
  let n = w2c $ currentByte s
  putChar n
  return s

eval :: Program -> ST.StateT VM IO ()
eval is = do
  m <- get
  if length is <= ip m
    then return ()
    else do
      let i = is !! ip m
      liftIO $ print m
      case i of
        Jump j -> do
          put $ incIp (evalJump j m)
          eval is
        Stmt s -> do
          put $ incIp (evalStmt s m)
          eval is
        IOStmt ios -> do
          newState <- liftIO $ evalIOStmt ios m
          put $ incIp newState
          eval is

initState = VM {dp = 0, ip = 0, memory = replicate 100 0}

run :: String -> IO VM
run is = execStateT (eval prog) initState
  where
    prog = case parse p "" is of
      Right is -> toProgram is
      Left _ -> error "Noo"

main :: IO VM
main = do
  args <- getArgs
  source <- readFile (head args)
  print source
  run source
