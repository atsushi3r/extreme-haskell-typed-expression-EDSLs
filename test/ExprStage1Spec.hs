module ExprStage1Spec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as M
import ExprStage1

main :: IO ()
main = defaultMain tests

-- テスト本体での利用例
tests :: TestTree
tests = testGroup "ExprStage1 Tests"
  [ testCase "plusThree evaluation (3 + 5 = 8)" $ do
      let env = M.empty
      let expr = EApply plusThree (EPrim (PInt 5))
      assertEVInt "plusThree" 8 (eval env expr)
  ]

-- EVInt 専用の検証ヘルパー
assertEVInt :: String -> Int -> Maybe EValue -> Assertion
assertEVInt label expectedVal actualExpr = case actualExpr of
  Just (EVInt n)
    | n == expectedVal -> pure ()
    | otherwise        -> assertFailure $ label ++ ": Expected EVInt " ++ show expectedVal ++ ", got EVInt " ++ show n
  Just other           -> assertFailure $ label ++ ": Expected EVInt " ++ show expectedVal ++ ", got " ++ show other
  Nothing              -> assertFailure $ label ++ ": Expected Just (EVInt " ++ show expectedVal ++ "), got Nothing"
