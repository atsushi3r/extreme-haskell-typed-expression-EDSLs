#!/usr/bin/env stack
{- stack
   --resolver nightly-2024-02-02
   script
   --package base,containers,prettyprinter
   --ghc-options -Wall
   --ghc-options -Wcompat
   --ghc-options -Wno-unused-top-binds
   --ghc-options -Wno-unused-imports
   --ghc-options -Wno-name-shadowing
-}
-- stack repl % --ghc-options=-fforce-recomp\ -fobject-code\ -O2

{-
 - Extreme Haskell: Typed Expression EDSLs (Part 1)
 - https://blog.jle.im/entry/extreme-haskell-typed-expression-edsls-1.html
 -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Map as M
import Prettyprinter as PP
import Text.Printf (printf)

data Prim = PInt Int | PBool Bool | PString String
  deriving (Eq, Show)

data Op = OPlus | OTimes | OLte | OAnd
  deriving (Eq, Show)

data Expr
  = EPrim Prim
  | EVar String
  | ELambda String Expr
  | EApply Expr Expr
  | EOp Op Expr Expr
  | ERecord (Map String Expr)
  | EAccess Expr String
  | EChoice String Expr
  | ECase Expr (Map String (String, Expr))
  deriving (Eq, Show)

-- (\x -> x * 3) 5
fifteen :: Expr
fifteen = ELambda "x"
    (EOp OTimes (EVar "x") (EPrim (PInt 3))) `EApply` EPrim (PInt 5)

badTypeExample :: Expr
badTypeExample =
  EOp OAnd (EPrim (PInt 1)) (EPrim (PInt 2))

recordExample :: Expr
recordExample =
  EOp
    OPlus
    ( EAccess
        ( ERecord $
            M.fromList
              [ ("value", EPrim (PInt 7)),
                ("label", EPrim (PString "found"))
              ]
        )
        "value"
    )
    (EPrim (PInt 1))

sumExample :: Expr
sumExample =
  ECase
    (EChoice "Found" (EPrim (PInt 7)))
    ( M.fromList
        [ ("Found", ("value", EOp OPlus (EVar "value") (EPrim (PInt 1)))),
          ("Missing", ("message", EPrim (PInt 0)))
        ]
    )

ppPrim :: Prim -> PP.Doc ann
ppPrim = \case
    PInt n      -> PP.pretty n
    PBool True  -> "true"
    PBool False -> "false"
    PString s   -> PP.pretty s

ppOp :: Op -> PP.Doc ann
ppOp = \case
    OPlus -> "+"
    OTimes -> "*"
    OLte -> "<="
    OAnd -> "&&"

ppExpr :: Bool -> Expr -> PP.Doc ann
ppExpr paren = \case
    EPrim p -> ppPrim p
    EVar v -> PP.pretty v
    ELambda n body ->
        wrap $ "\\" <> PP.pretty n <+> "->" <+> ppExpr False body
    EApply f x ->
        wrap $ ppExpr True f <+> ppExpr True x
    EOp o x y ->
        wrap $ ppExpr True x <+> ppOp o <+> ppExpr True y
    ERecord xs ->
        PP.encloseSep "{ " " }" ", " $
            [PP.pretty k <+> "=" <+> ppExpr False v | (k, v) <- M.toList xs]
    EAccess e k ->
        ppExpr True e <> "." <> PP.pretty k
    EChoice tag x ->
        wrap $ PP.pretty tag <+> ppExpr True x
    ECase x hs ->
        wrap $
            PP.sep
                [ "case" <+> ppExpr False x <+> "of"
                , PP.encloseSep "{ " " }" "; " $
                    [ PP.pretty tag <+> PP.pretty n <+> "->" <+> ppExpr False body
                    | (tag, (n, body)) <- M.toList hs
                    ]
                ]
  where
    wrap :: Doc ann -> Doc ann
    wrap
        | paren = PP.parens
        | otherwise = id

prettyExpr :: Expr -> PP.Doc ann
prettyExpr = ppExpr False

main :: IO ()
main = print 1


