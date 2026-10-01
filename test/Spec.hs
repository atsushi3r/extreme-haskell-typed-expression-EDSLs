module Main (main) where

import Test.Tasty

import qualified ExprStage1Spec
import qualified ExprStage2Spec

main :: IO ()
main = defaultMain $ testGroup "Extreme Haskell EDSL Tests"
    [ ExprStage1Spec.tests
    , ExprStage2Spec.tests
    ]
