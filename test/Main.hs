{-# LANGUAGE BlockArguments #-}

module Main (main) where

import BrainFuck (eval, parse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = do
  hspec do
    describe "eval" do
      prop "should output its input intact" do
        testInput . filter isAscii
      it "should evaluate hello world" do
        testHelloWorld
      it "should evaluate to 7" do
        testAirthmetics
      prop "should reverse input" do
        testReverse . filter isAscii . filter (/= '\NUL')

eof :: [Word8]
eof = [0]

testHelloWorld :: IO ()
testHelloWorld = do
  file <- readFile "test/programs/helloworld.bf"
  let program = parse file
      input = []

  let output = eval input program

  output `shouldBe` "Hello World!\n"

testAirthmetics :: IO ()
testAirthmetics = do
  file <- readFile "test/programs/arith.bf"
  let program = parse file
      input = []

  let output = eval input program

  output `shouldBe` "7"

testInput :: String -> IO ()
testInput input = do
  let program = parse $ concat $ replicate (length input) ",."
      input' = BS.unpack (C8.pack input)

  let output = eval input' program

  output `shouldBe` input

testReverse :: String -> IO ()
testReverse input = do
  file <- readFile "test/programs/reverse.bf"
  let program = parse file
      input' = BS.unpack (C8.pack input)

  let output = eval (input' <> eof) program

  output `shouldBe` reverse input
