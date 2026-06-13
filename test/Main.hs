{-# LANGUAGE BlockArguments #-}

module Main (main) where

import BrainFuck (eval, parse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Char

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

-- it "should encode right with rot13" do
--   testRot13 "sajt"

testHelloWorld :: IO ()
testHelloWorld = do
  file <- readFile "test/test1.bf"
  let program = parse file
      input = []

  let output = eval input program

  output `shouldBe` "Hello World!\n"

testAirthmetics :: IO ()
testAirthmetics = do
  file <- readFile "test/test2.bf"
  let program = parse file
      input = []

  let output = eval input program

  output `shouldBe` "7"

testRot13 :: String -> IO ()
testRot13 input = do
  file <- readFile "test/test3.bf"
  let program = parse file
      input' = BS.unpack (C8.pack input)

  let output = eval [0] program

  output `shouldBe` "7"

testInput :: String -> IO ()
testInput input = do
  let program = parse $ concat $ replicate (length input) ",."
      input' = BS.unpack (C8.pack input)

  let output = eval input' program

  output `shouldBe` input
