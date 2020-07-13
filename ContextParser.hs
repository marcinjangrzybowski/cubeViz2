{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ContextParser where

import Text.RawString.QQ

import Text.Parsec
import Text.Parsec.Expr

import Data.List
import Data.Bool
import Data.Either
import Data.Maybe

import Syntax

import ExprParser

import Data.Bifunctor

import qualified Data.Text as T


parseDef :: Parsec String [(String , String)] ()
parseDef =
  do spaces
     v <- agdaName
     spaces
     char ':'
     x <- manyTill anyChar (lookAhead (((try (const () <$> (agdaName *> spaces *> char ':') <|> eof)))))
     modifyState ((v , (T.unpack $ T.strip $ T.pack x)) :)
     return ()

parseDefs :: Parsec String [(String , String)] [(String , String)] 
parseDefs =
  do _ <- many (try parseDef)
     spaces
     eof
     defs <- getState
     return defs

parseContextRaw :: String -> Either String [(String , String)]
parseContextRaw = first show . (runParser parseDefs [] "")



parseDef2 :: (String , String) -> (Env , Context) ->  Either String (Env , Context)
parseDef2 (name , "Level") = Right . first (flip addLevelToEnv name)
parseDef2 (name , "I") = Right . second (flip addDimToContext name)
parseDef2 (name , rawDef) 
     | isPrefixOf "Type" rawDef = 
         let z = (T.unpack $ T.strip $ T.pack (drop 5 rawDef) )
         in \(env , ctx) ->
               (maybe (Left ("not a level: " ++ z)) (Right . (,ctx) . addBTyToEnv env name  ))
                  $ lookupLevel env z
     | otherwise =
           \(env , ctx) ->
               first show
                 (((env,) . flip (addVarToContext ctx) name)
                    <$> (runParser cTypeParser (env , ctx) "" rawDef))
               
    -- if rawDef == "Level"
    -- then second $ first $ (flip addLevelToEnv name)
    -- else (if isPrefixOf "Type" rawDef 
    --       then second $ first $ (\env -> (addBTyToEnv env name (BType 0)))
    --       else id
    --      )
  

parseContext :: String -> Either String (Env , Context) 
parseContext s =
  do rawDefs <- parseContextRaw s
     foldl (flip $ flip (>>=) . parseDef2) (Right (emptyEnv , emptyCtx)) rawDefs
     
  --Right [("a","1"),("a","1"),("a","1")]

cTypeParserDim0 :: (Env , Context) -> Parsec String (Env , Context) CType
cTypeParserDim0 (env , ctx) = 
  do sym <- agdaName
     case (lookupBType env sym) of
       Nothing -> fail ("not in scope: " ++ sym)
       Just x ->
         case (mkCType env ctx (x , [])) of
           Left z -> unexpected z
           Right cty -> return cty

cTypeParserDim1 :: (Env , Context) -> Parsec String (Env , Context) CType
cTypeParserDim1 (env , ctx) = 
  do string "_≡_"
     spaces
     char '{'
     char 'ℓ'
     char '}'
     spaces
     char '{'
     (CType (BType z) []) <- cTypeParserDim0 (env , ctx)
     let ty = (CType (BType z) [])
     char '}'
     spaces
     e0 <- parsecSnd env varIdentifier
     spaces
     e1 <- parsecSnd env varIdentifier
     let ty0 = getVarType ctx e0
     let ty1 = getVarType ctx e1
     if (ty0 == ty) && (ty1 == ty) 
     then return (CType (BType z) [(Var e0 [] , Var e1 [])])
     else fail "wrong type of ends"

cTypeParser :: Parsec String (Env , Context) CType
cTypeParser =
  do (env , ctx) <- getState
     (spaces *>
      (try (cTypeParserDim1 (env , ctx))
       <|> (cTypeParserDim0 (env , ctx))
      )
       <* spaces
       <* eof)
     
       
