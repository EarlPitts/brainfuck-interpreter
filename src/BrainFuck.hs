{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module BrainFuck (
  VM (..),
  Cell (..),
  Instr,
  mkVM,
  eval,
  evalWith,
  exec,
  execWith,
  execStep,
  toWords,
  fromWords,
  parse,
) where

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List (uncons)
import Data.List.Zipper
import Data.List.Zipper.Extended
import Data.Maybe
import Data.Word

data Instr
  = Incr
  | Decr
  | Backward
  | Forward
  | In
  | Out
  | JumpForward
  | JumpBack
  deriving (Eq)

type Program = [Instr]

newtype Cell = Cell {unCell :: Word8} deriving (Eq, Enum, Num)

instance Show Cell where
  show (Cell w) = show w

instance Semigroup Cell where
  (<>) (Cell w) (Cell w') = Cell (w + w')

instance Monoid Cell where
  mempty = Cell 0

instance Show Instr where
  show Incr = "+"
  show Decr = "-"
  show Backward = "<"
  show Forward = ">"
  show In = ","
  show Out = "."
  show JumpForward = "["
  show JumpBack = "]"

data VM = VM
  { tape :: Zipper Cell
  , program :: Zipper Instr
  , output :: [Word8]
  , input :: [Word8]
  }
  deriving (Show, Eq)

token :: Char -> Maybe Instr
token '+' = Just Incr
token '-' = Just Decr
token '[' = Just JumpForward
token ']' = Just JumpBack
token '>' = Just Forward
token '<' = Just Backward
token '.' = Just Out
token ',' = Just In
token _ = Nothing

parse :: String -> Program
parse = mapMaybe token

toWords :: String -> [Word8]
toWords = BS.unpack . C8.pack

fromWords :: [Word8] -> String
fromWords = C8.unpack . BS.pack

initTape :: Zipper Cell
initTape = fromList [Cell 0]

mkVM :: String -> String -> VM
mkVM programStr inputStr = let
    tape = initTape 
    output = []
    program = fromList $ parse programStr
    input = toWords inputStr
  in VM {..}

evalWith :: String -> String -> String
evalWith program input =
  fromWords (reverse vm.output)
    where
      vm = execState run (mkVM program input)

eval :: VM -> String
eval vm = fromWords (reverse endState.output)
  where
    endState = execState run vm

exec :: VM -> VM
exec = execState run

execStep :: VM -> VM
execStep = execState step

execWith :: String -> String -> VM
execWith program input =
    execState run (mkVM program input)

step :: State VM ()
step = do
  vm@VM{..} <- get
  unless (endp program) $ do
    let instr = cursor program
    case instr of
      Incr -> put vm{tape = replaceWith succ tape}
      Decr -> put vm{tape = replaceWith pred tape}
      Forward -> put vm{tape = right' tape}
      Backward -> put vm{tape = left tape}
      JumpForward -> when (cursor tape == 0) $ put vm{program = jumpForward (right program)}
      JumpBack -> when (cursor tape /= 0) $ put vm{program = jumpBack (left program)}
      In -> case uncons input of
        Nothing -> put vm{tape = replaceWith (const $ Cell 0) tape, input = []}
        Just (w,ws) -> put vm{tape = replaceWith (const $ Cell w) tape, input = ws}
      Out -> put vm{output = unCell (cursor tape) : output}
    proceed

run :: State VM ()
run = do
  prog <- gets program
  unless (endp prog)
    (step >> run)

proceed :: State VM ()
proceed = modify (\vm -> vm{program = right vm.program})

jumpForward :: Zipper Instr -> Zipper Instr
jumpForward = go (0 :: Int)
 where
  go cntr z = case safeCursor z of
    Nothing -> z
    Just JumpBack -> if cntr == 0 then z else go (pred cntr) (right z)
    Just JumpForward -> go (succ cntr) (right z)
    Just _ -> go cntr (right z)

jumpBack :: Zipper Instr -> Zipper Instr
jumpBack = go (0 :: Int)
 where
  go cntr z = case safeCursor z of
    Nothing -> z
    Just JumpForward -> if cntr == 0 then z else go (pred cntr) (left z)
    Just JumpBack -> go (succ cntr) (left z)
    Just _ -> go cntr (left z)
