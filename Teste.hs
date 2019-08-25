module Teste where

import Test.Tasty
import Test.Tasty.HUnit

import Defines
import CheckFunctions

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = (testGroup "Testes Projeto" [checkRowtest, checkMatrixtest])

             
checkRowtest = testGroup "checkRowTest" 
            [testCase "test1" (assertEqual "Test 1" False (checkRow [True, True, True, False])),
             testCase "test2" (assertEqual "Test 2" True (checkRow [True, True, True])),
             testCase "test3" (assertEqual "Test 3" True (checkRow [] ))           
             ]
             
checkMatrixtest = testGroup "checkMatrixTest"
            [testCase "test1" (assertEqual "Test 1" False (checkMatrix [[True], [True, True], [False]])),
             testCase "test2" (assertEqual "Test 2" True (checkMatrix [[True], [True], [True]])),
             testCase "test3" (assertEqual "Test 3" True (checkMatrix [])),
             testCase "test4" (assertEqual "Test 4" False (checkMatrix [[True,True], [True, True], [True, False]])),
             testCase "test5" (assertEqual "Test 5" True (checkMatrix [[]]))
             ]