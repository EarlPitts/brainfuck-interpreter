{-# LANGUAGE BlockArguments #-}

module Main (main) where

import BrainFuck
import Data.Char
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

testHelloWorld :: IO ()
testHelloWorld = do
  program <- readFile "test/programs/helloworld.bf"
  let input = ""

  let output = evalWith program input 

  output `shouldBe` "Hello World!\n"

testAirthmetics :: IO ()
testAirthmetics = do
  program <- readFile "test/programs/arith.bf"
  let input = ""

  let output = evalWith program input 

  output `shouldBe` "7"

testInput :: String -> IO ()
testInput input = do
  let program = concat $ replicate (length input) ",."

  let output = evalWith program input 

  output `shouldBe` input

testReverse :: String -> IO ()
testReverse input = do
  program <- readFile "test/programs/reverse.bf"

  let output = evalWith program (input <> ['\NUL'])

  output `shouldBe` reverse input
