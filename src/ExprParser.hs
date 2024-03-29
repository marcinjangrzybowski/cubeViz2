{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE ScopedTypeVariables #-}

module ExprParser where

import Text.RawString.QQ

import Text.Parsec
import Text.Parsec.Expr

import Data.List
import Data.Bool
import Data.Either
import Data.Maybe

import Data.Bifunctor

import Syntax

-- Convention
-- numbers sufxing parsers decribe diferent tiers
-- parser with 0 is meant to be used by other parsers
-- parser without suffix is standalone parser
-- TODO : rename standalone parsers to "standalone", o maybe wrap them in common function?

changeState
  :: forall m s u v a . (Functor m, Monad m)
  => (u -> v)
  -> (v -> u)
  -> ParsecT s u m a
  -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    mapState :: forall u v . (u -> v) -> State s u -> State s v
    mapState f st = st { stateUser = f (stateUser st) }

    mapReply :: forall u v . (u -> v) -> Reply s u a -> Reply s v a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    transform
      :: (State s u -> m (Consumed (m (Reply s u a))))
      -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))

parsecSnd :: a -> Parsec String b z -> Parsec String (a , b) z
parsecSnd a = changeState (a,) snd
  
implArg :: Parsec String Context z -> Parsec String Context z
implArg z = spaces *> (between (char '{') (spaces *> char '}')) z

notAbs :: Parsec String Context z -> Parsec String Context z
notAbs x = string "λ _ →" *> space  *> x

faceAbs :: FExpr -> Parsec String Context z -> Parsec String Context z
faceAbs ie x =
  do string "λ _ →"
     space
     -- ctx <- getState
     -- setState $ addSFConstraintToContext ctx undefined
     y <- x
     -- setState ctx 
     return y

abstT :: (Maybe String -> c -> c) -> Parsec String c z -> Parsec String c (Maybe String , z)
abstT f p =
  do spaces
     string "λ "
     ctx <- getState
     mbName <- (fmap Just agdaName) <|> (fmap (const Nothing) (string "_"))
     string " →"
     space
     setState (f mbName ctx)
     x <- p
     setState ctx 
     return (mbName , x)

     
abstr :: Parsec String Context z -> Parsec String Context (Maybe String , z)
abstr = abstT $ flip addDimToContext
  -- do spaces
  --    string "λ "
  --    ctx <- getState
  --    name <- agdaName
  --    string " →"
  --    space
  --    setState (addDimToContext ctx name)
  --    x <- p
  --    setState ctx 
  --    return (Just name , x)

agdaName = many1 (digit <|> letter <|> (oneOf ".₀₁₂₃₄₅₆₇₈₉'¹²"))
       
varIdentifier :: Parsec String Context VarIndex
varIdentifier = 
    do ctx <- getState
       x <- agdaName 
       let z = lookupVar ctx x
       case z of
         Nothing -> unexpected ("not defined: "++ x)
         Just i -> return (VarIndex i)
  

iExprVar :: Parsec String Context IExpr
iExprVar =
  do ctx <- getState
     x <- agdaName
     let z = lookupDim ctx x
     case z of
       Nothing -> unexpected ("not defined: "++ x)
       Just i -> return (dim i)
     
iExpr3 :: Parsec String Context IExpr
iExpr3 = do spaces
            e <- ((between (char '(') (spaces *> char ')') iExpr0) <|> iExprVar)
            spaces
            return e


iExpr2 :: Parsec String Context IExpr
iExpr2 = do spaces
            e <- ((char '~' *> spaces *> (neg <$> iExpr3))      
                  <|> iExpr3)
            spaces
            return e


iExpr1 :: Parsec String Context IExpr
iExpr1 = do spaces
            e <- ((try (do l <- iExpr2
                           char '∧'
                           r <- iExpr1
                           return (Syntax.min l r)
                          ))       
                  <|> iExpr2)
            spaces
            return e

iExpr0 :: Parsec String Context IExpr
iExpr0 = do spaces
            e <- ((try (do l <- iExpr1
                           char '∨'
                           r <- iExpr0
                           return (Syntax.max l r)
                         ))       
                  <|> iExpr1)
                      
            return e

iExpr :: Parsec String Context IExpr
iExpr = iExpr0
         <* spaces
         <* eof

iExprArg :: Parsec String Context IExpr
iExprArg = ((between (char '(') (spaces *> char ')') iExpr0) <|> iExprVar)

var1 :: Parsec String Context Expr
var1 =  do h <- varIdentifier
           ctx <- getState
           many1 space
           tl <- (sepEndBy iExprArg (many1 space))
           return (mkVar ctx h tl)
-- tail of expression should containt pairs of expressions of decrasing arrity, so that last expression have arrity 0
-- it should be achived by aplying nTh expression if CType tail to first n arguments.
-- if CType tails is [(f0,f1),(g0,g1),(h0,h1)]
---     and arguments are [x,y,z]
--  then resultng expression tail should be:
--     [(f0,f1),(g0 x ,g1 x),(h0 x y,h1 x y)]     


var0 :: Parsec String Context Expr
var0 = spaces *>  ((try var1) <|> (((flip Var) []) <$> (varIdentifier)))

var :: Parsec String Context Expr
var = var0 <* spaces  <* eof

typeP :: Parsec String Context ()
typeP = agdaName *> (pure ())

levelP :: Parsec String Context ()
levelP = agdaName *> (pure ())


partialPrimPOr :: IExpr -> Parsec String Context Partial
partialPrimPOr x =
  do string "primPOr"
     space
     implArg levelP
     space
     ia1 <- iExprArg
     space
     ia2 <- iExprArg
     space
     implArg (notAbs typeP)
     space
     p1 <- partialArg ia1
     space
     p2 <- partialArg ia2
     (either unexpected return (primPOr x ia1 ia2 p1 p2))


everyP :: [ Parsec String b c ] -> Parsec String b [ c ]
everyP [] = return []
everyP (x : xs) =
  do ys <- lookAhead $ try $ (everyP xs)
     y <- x
     eof
     return (y : ys)
    
-- testXX :: Parsec String () [String]
-- testXX = everyP [string "a b a" , (concat <$> many ((string "a") <|> (string " "))) , string "a a a" ]

partial0 :: IExpr -> Parsec String Context Partial
partial0 x =
      do spaces
         -- ctx <- getState
         ((partialPrimPOr x) <|> (partialConst x <$> faceAbs (toSubFace2 x) expr0))
      
partialArg :: IExpr -> Parsec String Context Partial
partialArg x = spaces *> between (char '(') (spaces *> char ')') (partial0 x)

partial :: IExpr -> Parsec String Context Partial
partial x = partial0 x <* spaces <* eof

hcomp :: Parsec String Context Expr
hcomp =
  do string "hcomp"
     space
     implArg (levelP)
     space
     implArg (typeP)
     space
     ie <- implArg (iExpr0)
     space
     (v , x) <- (spaces *> between (char '(') (spaces *> char ')') (abstr (partial0 ie)))
     space
     y <- exprArg
     return (HComp (v) x y)

hole :: Parsec String Context Expr
hole =
  do string "{!!}"
     return (Hole 0)

expr0 :: Parsec String Context Expr
expr0 = spaces *> ((try hcomp) <|> var0 <|> hole )

expr :: Parsec String Context Expr
expr = expr0 <* spaces <* eof

exprArg :: Parsec String Context Expr
exprArg = spaces *>  ((between (char '(') (spaces *> char ')') expr0))
            <|>
            ((((flip Var) []) <$> varIdentifier))

pathAbstrExpr0 ::  Parsec String Context LExpr
pathAbstrExpr0 = spaces *> (
  (fmap (first (\x -> [x])) (try $ abstr expr0))
    <|>
   fmap (\x -> ([] , x)) expr0 )

pathAbstrExprArg :: Parsec String Context LExpr
pathAbstrExprArg =
       spaces *>  ((between (char '(') (spaces *> char ')') pathAbstrExpr0))
            <|>
            (fmap (\x -> ([] , x)) (((flip Var) []) <$> varIdentifier))

  
parseExpr :: Context -> String -> Either ParseError Expr
parseExpr c = runParser expr c ""
