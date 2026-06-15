{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module BrainFuck (
  VM (..),
  Cell (..),
  Instr,
  eval,
  exec,
  parse,
) where

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
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
  deriving (Show, Eq)

type Program = [Instr]

type Input = [Word8]

newtype Cell = Cell {unCell :: Word8} deriving (Eq, Enum, Num)

instance Show Cell where
  show (Cell w) = show w

instance Semigroup Cell where
  (<>) (Cell w) (Cell w') = Cell (w + w')

instance Monoid Cell where
  mempty = Cell 0

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

eval :: Input -> Program -> String
eval input p = C8.unpack (BS.pack (reverse vm.output))
 where
  tape = fromList [Cell 0]
  program = fromList p
  output = []
  vm = execState run VM{..}

exec :: Input -> Program -> VM
exec input p =
  let
    tape = fromList [Cell 0]
    program = fromList p
    output = []
   in
    execState run VM{..}

run :: State VM ()
run = do
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
      In -> put vm{tape = replaceWith (const $ Cell (head input)) tape, input = tail input}
      Out -> put vm{output = unCell (cursor tape) : output}
    proceed
    run

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
