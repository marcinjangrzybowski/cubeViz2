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
import Data.Char

import Syntax

import ExprParser

import Data.Bifunctor

import qualified Data.Text as T

import DataExtra

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
parseDef2 (name , 'L':'e':'v':'e':'l' : _) = Right . first (flip addLevelToEnv name)
parseDef2 (name , 'I':[]) = Right . second (flip addDimToContext (Just name))
parseDef2 (name , 'I':' ':_) = Right . second (flip addDimToContext (Just name))
parseDef2 (name , rawDef) 
     | isPrefixOf "Type" rawDef = 
         let z' = (T.strip $ T.pack (drop 5 rawDef) )
             z = T.unpack $ T.takeWhile (not . isSpace) z'
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
  

addErrCtx :: String -> Either String a -> Either String a 
addErrCtx _ (Right x) = Right x
addErrCtx i (Left x) = Left (i ++ "\n" ++ x) 
  
parseContext :: String -> Either String (Env , Context) 
parseContext s =
  do rawDefs <- parseContextRaw s
     addErrCtx "while parsing context:" $
        foldl (flip $ flip (>>=) .
            (\x y -> addErrCtx ("on: " ++ show x) (parseDef2 x y))) (Right (emptyEnv , emptyCtx)) rawDefs
     
  --Right [("a","1"),("a","1"),("a","1")]

-- TODO : checking dimesnion of the types (as sanity check)

cTypeParserDim0 :: (Env , Context) -> Parsec String (Env , Context) CType
cTypeParserDim0 (env , ctx) = 
  do sym <- agdaName
     case (lookupBType env sym) of
       Nothing -> fail ("not in scope: " ++ sym)
       Just x ->
         case (mkCType env ctx (x , [])) of
           Left z -> fail ("unexpected: " ++ z)
           Right cty -> return cty


-- in those parsers we can be SHURE that TOP context do not contains dimensions,
-- and all dimensions are introduced by two parsers bellow


sideExpr env = changeState ((,) env) (snd) pathAbstrExprArg

cTypeParserId :: (Env , Context) -> Parsec String (Env , Context) CType
cTypeParserId (env , ctx) = 
  do string "_≡_"
     spaces
     implArg (levelP)
     spaces
     char '{'
     spaces
     putState (env , addDimToContext ctx (Just "noAbsDim") )
     (CType z l) <- cTypeParser0
     putState (env , ctx)
     spaces
     char '}'
     spaces
     e0 <- sideExpr env
     spaces
     e1 <- sideExpr env
     -- let ty0 = getVarType ctx e0
     -- let ty1 = getVarType ctx e1
     -- if (ty0 == ty) && (ty1 == ty) 
     --then
     let fcs = map (mapBoth $ arityForceRepair ctx (length l)) ((e0,e1) : (fmap (mapBoth $ iLam Nothing) l))
     return (CType z fcs)
     --else fail "wrong type of ends"

-- PathP {ℓ} (λ i₁ → _≡_ {ℓ} {A} (xy1 i₁) (yz2 i₁)) xy1 yz1

-- ss  : PathP {ℓ}
--       (λ i₁ →
--          PathP {ℓ} (λ i₂ → _≡_ {ℓ} {A} (sq5 i₁ i₂) (sq6 i₁ i₂)) (sq3 i₁)
--          (sq4 i₁))
--       sq1 sq2




-- ℓ
cTypeParserPathP :: (Env , Context) -> Parsec String (Env , Context) CType
cTypeParserPathP (env , ctx) = 
  do string "PathP"
     spaces
     implArg (levelP)
     spaces
     char '('
     spaces
     (mbSym , CType z l)
          <- abstT (second . flip addDimToContext) cTypeParser0
     spaces
     char ')'
     spaces
     e0 <- sideExpr env
     spaces
     e1 <- sideExpr env
     -- let ty0 = getVarType ctx e0
     -- let ty1 = getVarType ctx e1
     -- if (ty0 == ty) && (ty1 == ty)
     let fcs = map (mapBoth $ arityForceRepair ctx (length l)) ((e0,e1) : (fmap (mapBoth $ iLam mbSym) l))
     return (CType z fcs)
     -- else fail "wrong type of ends"


cTypeParser0 :: Parsec String (Env , Context) CType
cTypeParser0 =
  do (env , ctx) <- getState
     (spaces *>
      (    try (NotCType <$> string "A → B")
       <|> try (cTypeParserPathP (env , ctx))
       <|> try (cTypeParserId (env , ctx))
       <|> try (cTypeParserDim0 (env , ctx))
       -- <|> (NotCType <$> string "A → B")
      ) <* spaces)
     

cTypeParser :: Parsec String (Env , Context) CType
cTypeParser =
  do (env , ctx) <- getState
     (spaces *>
      (    try (NotCType <$> string "A → B")
       <|> try (cTypeParserPathP (env , ctx))
       <|> try (cTypeParserId (env , ctx))
       <|> (cTypeParserDim0 (env , ctx))
       
      )
       <* spaces
       <* optional (string "(not in scope)")
       <* eof)
     
       
