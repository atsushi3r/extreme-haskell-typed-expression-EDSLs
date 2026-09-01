{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExprStage1 where

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

plusThree :: Expr
plusThree = ELambda "x" (EOp OPlus (EVar "x") (EPrim (PInt 3)))

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

data EValue
    = EVInt    Int
    | EVBool   Bool
    | EVString String
    | EVFun    (EValue -> Maybe EValue)
    | EVRecord (Map String EValue)
    | EVChoice String EValue

instance Show EValue where
    show = \case
        EVInt n        -> "EVInt "    ++ show n
        EVBool b       -> "EVBool "   ++ show b
        EVString s     -> "EVString " ++ show s
        EVFun {}       -> "EVFun <function>"
        EVRecord xs    -> "EVRecord " ++ show xs
        EVChoice tag x -> "EVChoice " ++ show tag ++ " (" ++ show x ++ ")"

eval :: Map String EValue -> Expr -> Maybe EValue
eval env = \case
    EPrim p -> evalPrim p
    EVar v -> M.lookup v env
    ELambda n body -> pure (EVFun (\x -> eval (M.insert n x env) body))
    EApply f x -> eval env f >>= \case
        EVFun f' -> eval env x >>= f'
        _ -> Nothing
    EOp o x y -> do
        u <- eval env x
        v <- eval env y
        case (u, v) of
            (EVInt a, EVInt b) -> case o of
                OPlus  -> pure (EVInt (a + b))
                OTimes -> pure (EVInt (a * b))
                OLte   -> pure (EVBool (a <= b))
                OAnd   -> Nothing
            (EVBool a, EVBool b) -> case o of
                OAnd -> pure (EVBool (a && b))
                _    -> Nothing
            _ -> Nothing
    ERecord xs -> EVRecord <$> traverse (eval env) xs
    EAccess e k -> do
        EVRecord xs <- eval env e
        M.lookup k xs
    EChoice tag x -> EVChoice tag <$> eval env x
    ECase x hs -> do
        EVChoice tag payload <- eval env x
        (n, body) <- M.lookup tag hs
        eval (M.insert n payload env) body

evalPrim :: Prim -> Maybe EValue
evalPrim = \case
    PInt n -> pure (EVInt n)
    PBool b -> pure (EVBool b)
    PString s -> pure (EVString s)

