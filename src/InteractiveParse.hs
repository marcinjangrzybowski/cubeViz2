{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module InteractiveParse where

import Syntax

import Data.List

import ExprParser

import ContextParser

import Data.Bifunctor

import Data.Maybe

consToHead :: a -> [[a]] -> [[a]]
consToHead a [] = [[a]]
consToHead a (x : xs) = (a : x) : xs
  
splitsBy :: Eq a => [[a]] -> [a] -> Either [a] [[a]]
splitsBy [] y = Right [y]
splitsBy (x : xs) [] = Left x
splitsBy (x : xs) (y : ys) = 
  if (isPrefixOf x (y : ys))
  then ([] :) <$> splitsBy xs (drop (length x) (y : ys)) 
  else (consToHead y) <$> (splitsBy (x : xs) ys)


sectionsHeads :: [String]
sectionsHeads =
  fmap snd [(True , "Goal:")
  ,(True , "Elaborates to:")
  ,(False , "———— Boundary ——————————————————————————————————————————————")
  ,(True , "————————————————————————————————————————————————————————————")
  ,(False , "———— Constraints ———————————————————————————————————————————")
  ]

sectionsHeads' :: [String]
sectionsHeads' =
  fmap snd [(True , "Goal:")
  ,(True , "Elaborates to:")
  ,(True , "————————————————————————————————————————————————————————————")
  ]


rawSections :: String -> Either String (String,String,String,String,String)
rawSections x =
  case (splitsBy sectionsHeads x) of
    Right [_ , s1 , s2 , s3 , s4 , s5] -> Right (s1 , s2 , s3 , s4 , s5)
    Right _ -> Left "fatal error in rawSections"
    Left y -> Left ("section: " ++ y ++ " is missing from input")

rawSections' :: String -> Either String (String,String,String)
rawSections' x =
  case (splitsBy sectionsHeads' x) of
    Right [_ , s1 , s2 , s4] -> Right (s1 , s2 ,  s4)
    Right _ -> Left "fatal error in rawSections"
    Left y -> Left ("section: " ++ y ++ " is missing from input")


data SessionState = SessionState (Env , Context) [(Expr , Expr)] BType Expr




ssEnvExpr :: SessionState -> ((Env, Context), Expr)
ssEnvExpr (SessionState ee _ _ e) = (ee , e)

ssSetExpression :: Maybe (Env, Context) -> SessionState -> Expr -> SessionState
ssSetExpression mbEE (SessionState ee si bt _) e = (SessionState (fromMaybe ee mbEE) si bt e)

ssAddToContext :: SessionState -> CType -> (SessionState , Int)
ssAddToContext = undefined

freshSessionState :: Int -> SessionState 
freshSessionState dim =
  SessionState
    (singleLevelAndTypeEnv , freshCtx dim)
    (replicate dim (undefined , undefined))
    (BType 0)
    (Hole 0)


instance Show SessionState where
   show (SessionState (env , ctx) bndrs bType e) =
     show env ++ "\n"
     ++ toString (env , emptyCtx) ctx
     ++ "\n"
     ++ "Expr:\n" ++ (toString (env , ctx) e)
     ++ "\n"

  
parseInteractive :: String -> Either String SessionState
parseInteractive x =
  do (goalRaw , termRaw , boundaryRaw , contextRaw , constraintsRaw) <- rawSections x
     (env , ctx) <- parseContext contextRaw
     currExpr <- first show $ parseExpr ctx termRaw
     return (SessionState (env , ctx) [] undefined currExpr)

parseInteractive' :: String -> Either String SessionState
parseInteractive' x =
  do (goalRaw , termRaw , contextRaw ) <- rawSections' x
     (env , ctx) <- parseContext contextRaw
     currExpr <- first show $ parseExpr ctx termRaw
     return (SessionState (env , ctx) [] undefined currExpr)



