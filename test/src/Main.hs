module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))


tests :: TestTree
tests = testGroup "Tests"
  [ testCase "true" $
      True @?= True
  ]

--tests:: TestTree
--tests = testGroup "Token"
--  [ testCase "testToken001" testToken001
--  , testCase "testToken002" testToken002
--  , testCase "testToken003" $
--      asToken "(" @?= Just LeftBrace
--  , testCase "testToken004" $
--      asToken "+" @?= Just Plus
--  , testCase "testToken005" $
--      asToken "*" @?= Nothing

--  , testCase "testNumber001" $
--      asNumber "123" @?= Just (Number 123)
--  , testCase "testNumber002" $
--      asNumber "123e" @?= Nothing
--  , testCase "testNumber003" $
--      asNumber "e123" @?= Nothing
--  , testCase "testSign+" $
--      asSign "+" @?= Just Plus
--  , testCase "testSign-" $
--      asSign "-" @?= Just Minus
--  , testCase "testSign*" $
--      asSign "*" @?= Nothing
--  , testCase "testBraceR" $
--      asBrace ")" @?= Just RightBrace
--  , testCase "testBraceL" $
--      asBrace "(" @?= Just LeftBrace
--  , testCase "testBrace*" $
--      asBrace "*" @?= Nothing

--  , testCase "testTokenize001" testTokenize001
--  , testCase "testTokenize002" testTokenize002
--  , testCase "testTokenize003" $
--      tokenize "" @?= Just []
--  , testCase "testTokenize004" $
--      tokenize "123 + ( - 3345 )" @?= Just [Number 123, Plus, LeftBrace, Minus, Number 3345, RightBrace]
--  , testCase "testTokenize005" $
--      tokenize "123 + e" @?= Nothing
--  ]

--testToken001 :: Assertion
--testToken001 =
--    asToken "123" @?= Just (Number 123)

--testToken002 :: Assertion
--testToken002 =
--    asToken "abc" @?= Nothing

--testTokenize001 :: Assertion
--testTokenize001 =
--    tokenize "1 + 2" @?= Just [Number 1, Plus, Number 2]

--testTokenize002 :: Assertion
--testTokenize002 =
--    tokenize "1 + ( 7 - 2 )" @?= Just [Number 1, Plus, LeftBrace, Number 7, Minus, Number 2, RightBrace]


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ tests ]
