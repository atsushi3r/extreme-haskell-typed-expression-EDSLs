module ExprStage2Spec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as M
import ExprStage2

tests :: TestTree
tests = testGroup "ExprStage2 Tests"
    [ testEval
    , testPretty
    ]

testEval :: TestTree
testEval = testGroup "eval"
    [ testCase "fifteen evaluates to 15" $
        eval M.empty fifteen `assertEVInt` 15
    , testCase "plusThree applies correctly" $
        eval M.empty (EApply plusThree (EPrim (PInt 4))) `assertEVInt` 7
    , testCase "lookup variable in environment with matching type" $ do
        let env = M.singleton "x" (SomeValue STInt (EVInt 10))
        eval env (EVar STInt "x") `assertEVInt` 10
    , testCase "type mismatch in env returns Nothing" $ do
        let env = M.singleton "x" (SomeValue STBool (EVBool True))
        assertNothing $ eval env (EVar STInt "x")
    , testCase "boolean operator OAnd" $
        eval M.empty (EOp OAnd (EPrim (PBool True)) (EPrim (PBool False))) `assertEVBool` False
    ]

testPretty :: TestTree
testPretty = testGroup "prettyExpr"
  [ testCase "pretty print fifteen" $ do
      let doc = prettyExpr fifteen
      show doc @?= "(\\x -> x * 3) 5"

  , testCase "pretty print plusThree" $ do
      let doc = prettyExpr plusThree
      show doc @?= "\\x -> x + 3"
  ]

-- | Maybe (EValue TInt) が期待する Int 値であることを検証
assertEVInt :: Maybe (EValue TInt) -> Int -> Assertion
assertEVInt res expected = case res of
    Just (EVInt n) -> n @?= expected
    _              -> assertFailure $ "Expected Just (EVInt " ++ show expected ++ ")"

-- | Maybe (EValue TBool) が期待する Bool 値であることを検証
assertEVBool :: Maybe (EValue TBool) -> Bool -> Assertion
assertEVBool res expected = case res of
  Just (EVBool b) -> b @?= expected
  _               -> assertFailure $ "Expected Just (EVBool " ++ show expected ++ ")"

-- | 結果が Nothing であることを検証
assertNothing :: Maybe a -> Assertion
assertNothing res = case res of
  Nothing -> pure ()
  Just _  -> assertFailure "Expected Nothing, but got Just"

