module Teste where

import Test.Tasty
import Test.Tasty.HUnit

import Defines
import CheckFunctions
import InputOutput

main :: IO ()
main = do
  contents <- readFile "input.txt"
  defaultMain $ tests contents

tests :: String -> TestTree
tests contents = (testGroup "Testes Projeto" [checkRowtest, checkMatrixtest, createNewMatrixTest, printaSolLineTest, printLineTest, strToIntTest, splitStrTest, splitNumbersTest, processeFileTest contents])

             
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
createNewMatrixTest = testGroup "createNewMatrixTest"
            [testCase "test1" (assertEqual "Test 1" (show [[True], [False, False], [False]]) (show (createNewMatrix [[True], [True], [False]] 1 0 [False,False]))),
             testCase "test2" (assertEqual "Test 2" "[]" (show (createNewMatrix [] 4 0 [True,False]))),
             testCase "test3" (assertEqual "Test 3" (show [[True], [False, True], [False]]) (show (createNewMatrix [[True], [], [False]] 1 0 [False,True]))),
             testCase "test4" (assertEqual "Test 4" (show [[True], [True], [False]]) (show (createNewMatrix [[True], [True], [False]] 3 0 [False,False])))
             ]

printaSolLineTest = testGroup "printaSolLineTest"
            [testCase "test1" (assertEqual "Test 1" " 1 3 7 " (printaSolLine [1,3,7] " " )),
             testCase "test2" (assertEqual "Test 2" " " (printaSolLine [] " " ))
             ]


printLineTest = testGroup "printaLineTest"
            [testCase "test1" (assertEqual "Test 1" " False True " (printaLine [False, True] " " )),
             testCase "test2" (assertEqual "Test 2" " " (printaLine [] " " ))
             ]             

strToIntTest = testGroup "strToIntTest"
            [testCase "test1" (assertEqual "Test 1" 1234 (strToInt "1234" )),
             testCase "test2" (assertEqual "Test 2" 1 (strToInt "1" ))
             ] 


splitStrTest = testGroup "splitStrTest"
            [testCase "test1" (assertEqual "Test 1" ["1","2","3","4"] (splitStr ' ' "1 2 3 4" )),
             testCase "test2" (assertEqual "Test 2" ["1","2","3","4"] (splitStr ',' "1,2,3,4" )),
             testCase "test3" (assertEqual "Test 3" ["1,2,3,4"] (splitStr ' ' "1,2,3,4" )),
             testCase "test2" (assertEqual "Test 2" [] (splitStr ',' "" ))
             ] 

splitNumbersTest = testGroup "splitNumbersTest"
            [testCase "test1" (assertEqual "Test 1" [[1,2,3,4]] (splitNumbers ' ' ["1 2 3 4"] )),
             testCase "test2" (assertEqual "Test 2" [[1,2,3,4], [14, 2554, 8, 10]] (splitNumbers ' ' ["1 2 3 4", "14 2554 8 10"] )),
             testCase "test3" (assertEqual "Test 3" [[1234], [1455810]] (splitNumbers ',' ["1234", "1455810"] )),
             testCase "test4" (assertEqual "Test 4" [[]] (splitNumbers ',' [[]] )),
             testCase "test5" (assertEqual "Test 5" [] (splitNumbers ' ' [] )),
             testCase "test6" (assertEqual "Test 6" [[2,4], [45,8,0]] (splitNumbers '1' ["214", "451810"] ))
             ] 


processeFileTest contents = testGroup "processeFileTest"
            [testCase "test1" (assertEqual "Test 1" [[1,2],[3,4]] (processeFile "1 2\n3 4" )),
             testCase "test2" (assertEqual "Test 2" [[1,2,3,4], [14, 2554, 8, 10]] (processeFile "1 2 3 4\n14 2554 8 10" )),
             testCase "test3" (assertEqual "Test 3" [[1234], [1455810]] (processeFile "1234\n1455810" )),
             testCase "test4" (assertEqual "Test 4" [] (processeFile "" )),
             testCase "test5" (assertEqual "Test 5" [[]] (processeFile "\n" )),
             testCase "test6" (assertEqual "Test 6" [[1]] (processeFile "1\n" )),
             testCase "test7" (assertEqual "Test 7" [[1], [4,5], [234, 8, 13, 0], [0] , [0, 7]] (processeFile contents ))
             ] 