
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax where


import Data.List


data IExpr =
   Min IExpr IExpr |
   Max IExpr IExpr |
   Dim Int |
   Neg IExpr
   deriving Show

type Partial = [(IExpr , Expr)]

data Expr =
  HComp Partial Expr
  | Var Int [IExpr]
  deriving Show


type Type = Expr


data Context = Context [(String,Type)] [(String , Maybe Bool)]
                -- deriving Show

addTyToContext :: Context -> Expr -> String -> Context
addTyToContext (Context t d) tm name = Context ((name , tm) : t) d

addDimToContext :: Context -> String -> Context
addDimToContext (Context t d) name = Context t ((name , Nothing) : d)


lookupDim :: Context -> String -> Maybe Int
lookupDim (Context _ l) x = elemIndex x (map fst l)

lookupVar :: Context -> String -> Maybe Int
lookupVar (Context l _) x = elemIndex x (map fst l)

instance Show Context where
   show (Context ty dims) =
     "Types:\n" ++ (intercalate "\n" (map (\(nm, val) -> nm ++ " : " ++ show val) ty)) 
     ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))

type Boundary = [((Int,Bool),Expr)]


partialConst :: Expr -> Partial
partialConst = undefined

emptyC = Context [] []
