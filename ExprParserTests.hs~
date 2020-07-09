{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ExprParser where

import Text.RawString.QQ

import Text.Parsec
import Text.Parsec.Expr

import Data.List
import Data.Bool
import Data.Either

import Syntax

assoc :: String
assoc = [r|hcomp {x.A.ℓ} {x.A} {~ i ∨ i}
(λ j →
   primPOr {x.A.ℓ} (~ i) i {λ _ → x.A} (λ _ → x)
   (λ _ →
      hcomp {x.A.ℓ} {x.A} {~ j ∨ j ∨ k}
      (λ k₁ →
         primPOr {x.A.ℓ} (~ j) (j ∨ k) {λ _ → x.A} (λ _ → q k)
         (primPOr {x.A.ℓ} j k {λ _ → x.A} (λ _ → r k₁) (λ _ → r (j ∧ k₁))))
      (q (j ∨ k))))
(hcomp {x.A.ℓ} {x.A} {~ i ∨ i ∨ ~ k}
 (λ k₁ →
    primPOr {x.A.ℓ} (~ i) (i ∨ ~ k) {λ _ → x.A} (λ _ → x)
    (primPOr {x.A.ℓ} i (~ k) {λ _ → x.A} (λ _ → q (k ∧ k₁))
     (λ _ → p i)))
 (p i))|]

pent :: String
pent = [r|  hcomp {ℓ} {A} {(~ i ∨ i) ∨ ~ j ∨ j}
   (λ i' →
      primPOr {ℓ} (~ i ∨ i) (~ j ∨ j) {λ _ → A}
      ( primPOr {ℓ} (~ i) i {λ _ → A}
         (λ _ →
            hcomp {ℓ} {A} {~ j ∨ j}
            (λ j₁ →
               primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
               (λ _ →
                  hcomp {ℓ} {A} {~ j₁ ∨ j₁}
                  (λ j₂ →
                     primPOr {ℓ} (~ j₁) j₁ {λ _ → A} (λ _ → y)
                     (λ _ →
                        hcomp {ℓ} {A} {~ j₂ ∨ j₂ ∨ ~ i'}
                        (λ k →
                           primPOr {ℓ} (~ j₂) (j₂ ∨ ~ i') {λ _ → A} (λ _ → r (~ i'))
                           (primPOr {ℓ} j₂ (~ i') {λ _ → A} (λ _ → s k) (λ _ → s (j₂ ∧ k))))
                        (r (j₂ ∨ ~ i'))))
                  (hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i'}
                   (λ k →
                      primPOr {ℓ} (~ j₁) (j₁ ∨ i') {λ _ → A} (λ _ → y)
                      (primPOr {ℓ} j₁ i' {λ _ → A} (λ _ → r (~ i' ∧ k)) (λ _ → q j₁)))
                   (q j₁))))
            (p j))
         (λ _ →
            hcomp {ℓ} {A} {~ j ∨ j}
            (λ j₁ → primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → s j₁))
            (hcomp {ℓ} {A} {~ j ∨ j}
             (λ j₁ →
                primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
                (λ _ →
                   hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i'}
                   (λ k →
                      primPOr {ℓ} (~ j₁) (j₁ ∨ i') {λ _ → A} (λ _ → q i')
                      (primPOr {ℓ} j₁ i' {λ _ → A} (λ _ → r k) (λ _ → r (j₁ ∧ k))))
                   (q (j₁ ∨ i'))))
             (hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i'}
              (λ k →
                 primPOr {ℓ} (~ j) (j ∨ ~ i') {λ _ → A} (λ _ → x)
                 (primPOr {ℓ} j (~ i') {λ _ → A} (λ _ → q (i' ∧ k)) (λ _ → p j)))
              (p j))))
         )
      (primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → v)))
   (hcomp {ℓ} {A} {~ j ∨ j}
    (λ j₁ →
       primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
       (λ _ →
          hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i}
          (λ k →
             primPOr {ℓ} (~ j₁) (j₁ ∨ i) {λ _ → A}
             (λ _ →
                hcomp {ℓ} {A} {~ i ∨ i}
                (λ j₂ → primPOr {ℓ} (~ i) i {λ _ → A} (λ _ → y) (λ _ → r j₂))
                (q i))
             (primPOr {ℓ} j₁ i {λ _ → A} (λ _ → s k) (λ _ → s (j₁ ∧ k))))
          (hcomp {ℓ} {A} {(~ j₁ ∧ ~ i) ∨ j₁ ∨ i}
           (λ j₂ →
              primPOr {ℓ} (~ j₁ ∧ ~ i) (j₁ ∨ i) {λ _ → A} (λ _ → y) (λ _ → r j₂))
           (q (j₁ ∨ i)))))
    (hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i}
     (λ k →
        primPOr {ℓ} (~ j) (j ∨ ~ i) {λ _ → A} (λ _ → x)
        (primPOr {ℓ} j (~ i) {λ _ → A}
         (λ _ →
            hcomp {ℓ} {A} {(~ i ∨ ~ k) ∨ i ∧ k}
            (λ j₁ →
               primPOr {ℓ} (~ i ∨ ~ k) (i ∧ k) {λ _ → A} (λ _ → y) (λ _ → r j₁))
            (q (i ∧ k)))
         (λ _ → p j)))
     (p j)))|]

-- Convention
-- numbers sufxing parsers decribe diferent tiers
-- parser with 0 is meant to be used by other parsers
-- parser without suffix is standalone parser
-- TODO : rename standalone parsers to "standalone", o maybe wrap them in common function?


  
implArg :: Parsec String Context z -> Parsec String Context z
implArg z = spaces *> (between (char '{') (spaces *> char '}')) z

notAbs :: Parsec String Context z -> Parsec String Context z
notAbs x = string "λ _ →" *> space  *> x

abstr :: Parsec String Context z -> Parsec String Context z
abstr p =
  do spaces
     string "λ "
     ctx <- getState
     name <- agdaName
     string " →"
     space
     setState (addDimToContext ctx name)
     x <- p
     setState ctx 
     return x

agdaName = many1 (digit <|> letter <|> (oneOf ".₀₁₂₃₄₅₆₇₈₉'"))
       
varIdentifier :: Parsec String Context Int
varIdentifier = 
    do ctx <- getState
       x <- agdaName 
       let z = lookupVar ctx x
       case z of
         Nothing -> unexpected ("not defined: "++ x)
         Just i -> return i
  

iExprVar :: Parsec String Context IExpr
iExprVar =
  do ctx <- getState
     x <- agdaName
     let z = lookupDim ctx x
     case z of
       Nothing -> unexpected ("not defined: "++ x)
       Just i -> return (Dim i)
     
iExpr3 :: Parsec String Context IExpr
iExpr3 = do spaces
            e <- ((between (char '(') (spaces *> char ')') iExpr0) <|> iExprVar)
            spaces
            return e


iExpr2 :: Parsec String Context IExpr
iExpr2 = do spaces
            e <- ((char '~' *> spaces *> (Neg <$> iExpr3))      
                  <|> iExpr3)
            spaces
            return e


iExpr1 :: Parsec String Context IExpr
iExpr1 = do spaces
            e <- ((try (do l <- iExpr2
                           char '∧'
                           r <- iExpr1
                           return (Min l r)
                          ))       
                  <|> iExpr2)
            spaces
            return e

iExpr0 :: Parsec String Context IExpr
iExpr0 = do spaces
            e <- ((try (do l <- iExpr1
                           char '∨'
                           r <- iExpr0
                           return (Max l r)
                         ))       
                  <|> iExpr1)
                      
            return e

iExpr :: Parsec String Context IExpr
iExpr = iExpr0 <* spaces  <* eof

iExprArg :: Parsec String Context IExpr
iExprArg = ((between (char '(') (spaces *> char ')') iExpr0) <|> iExprVar)

var1 :: Parsec String Context Expr
var1 =  do h <- varIdentifier
           many1 space
           tl <- sepBy iExprArg (many1 space)
           return (Var h tl)

var0 :: Parsec String Context Expr
var0 = ((try var1) <|> (((flip Var) []) <$> (spaces *> varIdentifier)))

var :: Parsec String Context Expr
var = var0 <* spaces  <* eof

typeP :: Parsec String Context ()
typeP = var0 *> (pure ())

levelP :: Parsec String Context ()
levelP = var0 *> (pure ())


partialPrimPOr :: Parsec String Context Partial
partialPrimPOr =
  do string "primPOr"
     space
     implArg (levelP)
     space
     iExprArg
     space
     iExprArg
     space
     implArg (notAbs typeP)
     space
     partialArg
     space
     partialArg
     return []

partial0 :: Parsec String Context Partial
partial0 = do spaces
              e <- ((try (partialPrimPOr))       
                    <|> (partialConst <$> (notAbs expr0)))
                
              return []

partialArg :: Parsec String Context Partial
partialArg = spaces *> between (char '(') (spaces *> char ')') partial0

partial :: Parsec String Context Partial
partial = partial0 <* spaces <* eof

hcomp :: Parsec String Context Expr
hcomp =
  do string "hcomp"
     space
     implArg (levelP)
     space
     implArg (typeP)
     space
     implArg (iExpr0)
     space
     (spaces *> between (char '(') (spaces *> char ')') (abstr partial0))
     space
     exprArg
     return (Var 0 [])


expr0 :: Parsec String Context Expr
expr0 = spaces *> ((try hcomp) <|> var0 )

expr :: Parsec String Context Expr
expr = expr0 <* spaces <* eof

exprArg :: Parsec String Context Expr
exprArg = (try (spaces *> between (char '(') (spaces *> char ')') expr0))
            <|>
            (spaces *> (((flip Var) []) <$> varIdentifier))

  
parseExpr :: Context -> String -> Either ParseError Expr
parseExpr c s = Right (Var 0 [])




--tests:

runParserTests :: Show b => Parsec String a b
                   -> [(a , Bool , String)]
                   -> [((a , Bool , String) , Either (ParseError) b)]
runParserTests p = map (\(x , b , y) -> ((x , b , y) , (runParser p x "" y)))


onlyTestRest :: Show b => [((a , Bool , String) , Either (ParseError) b)]
                   -> [(Bool , (String , String ))]
onlyTestRest = map (\((_ , b , y) , e) -> ( b == (isRight e) , (y , (either show show) e)))

showAllTestRes :: Show b => [((a , Bool , String) , Either (ParseError) b)]
                   -> String
showAllTestRes =  

   ( ++ "\n") . intercalate "\n\n" . (map (\((x , b , y) , rslt ) ->
                                
                              let info = either
                                        (\z -> y ++ "\n" ++ show z)
                                        (\z -> y  ++ "\n" ++ (show z))
                                        rslt
                                  pass = (isRight rslt) == b
                                  msg = if pass then "OK  -- " else "BAD -- "
                               in msg ++ info
                            ))

showRes :: [(Bool , (String , String ))]
                   -> String
showRes =  
   ( ++ "\n") . intercalate "\n\n" . (map (\(pass , (raw , info) ) ->
                                
                              let msg = if pass then "OK  -- " else "BAD -- "
                               in msg ++ raw ++ "\n" ++ info
                            ))

-- IExpr parser


ok :: (a , b) -> (a , Bool , b)
ok (a , b) = (a , True , b)

bad :: (a , b) -> (a , Bool , b)
bad (a , b) = (a , False , b)

iExprParsingTests =
  let c = foldl addDimToContext emptyC ["j","k","l","i"]
  in
  [ok (c , "j")
  ,ok (c , "(j)")
  ,ok (c , "j ∨ k")
  ,ok (c , "j ∧ k ∧ l")
  ,ok (c , "j ∨ k ∧ l")
  ,ok (c , "j ∨ k ∨ l")
  ,ok (c , "j ∧    k ∨ ~ l")
  ,ok (c , "( j ∨ ( j     ∨ (i ∧ ~ (~       k) )))")
  ,ok (c , "~ (~ (( j ∨ ~ (i ∧ k))))")
  ,ok (c , "~ j ∧ j ∨ ~ k")]


runIExprTests = (putStr . (showAllTestRes . runParserTests iExpr)) iExprParsingTests

varParsingTests =
  let c0 = foldl addDimToContext emptyC ["j","k","l","i"]
      c = addTyToContext c0 (Var 0 [])  "p"
  in
  [ok (c , "p")
  ,ok (c , "p j")
  ,ok (c , "p (j)")
  ,ok (c , "p (j ∨ k) i")
  ,bad (c , "p j ∨ k (~ i)")
  ,bad (c , "p (j ∨ k) ~ i")
  ,ok (c , "p (j ∨ k) (~ i)")
  ]


runVarTests = (putStr . (showAllTestRes . runParserTests var)) varParsingTests



partialParsingTests =
  let c0 = foldl addDimToContext emptyC ["j","k","kk","l","i"]
      c = foldl (\cc -> addTyToContext cc (Var 0 []) ) c0
            [ "p", "A", "r", "ℓ", "q", "x","y","z","w","v"] 
  in
  [
    ok (c ,"primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x ) (λ _ → v)")
  , bad (c ,"primPOr {ℓ} (~ j) j {λ _ → A} (λ _ →x) (λ _ → v)")
  , ok (c ,"primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → v)")
  , ok (c ,"primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x i j) (λ _ → v)")
  , ok (c ,"primPOr {ℓ} (~ j) j {λ _ → A}      (λ _ → x i j) (λ _ → v)")
  , ok (c ,"primPOr {ℓ} (~ j) j     {λ _ → A} (λ _ → x i j) (λ _ → v)")
  , ok (c , [r|primPOr {ℓ} (~ j) (j ∨ k) {λ _ → x} (λ _ → q k)
             (primPOr {ℓ} j k {λ _ → x} (λ _ → r k) (λ _ → r (j ∧ k)))|])
  , ok (c , [r|primPOr {ℓ} (~ j) (j ∨ kk) {λ _ → x} (λ _ → q k)
             (primPOr {ℓ} j k {λ _ → x} (λ _ → r k) (λ _ → r (j ∧ k)))|])
   
  ]

runPartialTests = (putStr . (showAllTestRes . runParserTests partial))
                      partialParsingTests

abstrParsingTests =
  let c0 = foldl addDimToContext emptyC ["j","k","kk","l","i"]
      c = foldl (\cc -> addTyToContext cc (Var 0 []) ) c0
            [ "p", "A", "r", "ℓ", "q", "s", "x","y","z","w","v"] 
  in
  [
    ok (c , "λ k₁ → primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x ) (λ _ → v)")
  , ok (c , [r|
              λ j₁ →
       primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
       (λ _ →
          hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i}
          (λ k →
             primPOr {ℓ} (~ j₁) (j₁ ∨ i) {λ _ → A}
             (λ _ →
                hcomp {ℓ} {A} {~ i ∨ i}
                (λ j₂ → primPOr {ℓ} (~ i) i {λ _ → A} (λ _ → y) (λ _ → r j₂))
                (q i))
             (primPOr {ℓ} j₁ i {λ _ → A} (λ _ → s k) (λ _ → s (j₁ ∧ k))))
          (hcomp {ℓ} {A} {(~ j₁ ∧ ~ i) ∨ j₁ ∨ i}
           (λ j₂ →
              primPOr {ℓ} (~ j₁ ∧ ~ i) (j₁ ∨ i) {λ _ → A} (λ _ → y) (λ _ → r j₂))
           (q (j₁ ∨ i))))|])
  , ok (c , [r|   λ i' →
      primPOr {ℓ} (~ i ∨ i) (~ j ∨ j) {λ _ → A}
      ( primPOr {ℓ} (~ i) i {λ _ → A}
         (λ _ →
            hcomp {ℓ} {A} {~ j ∨ j}
            (λ j₁ →
               primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
               (λ _ →
                  hcomp {ℓ} {A} {~ j₁ ∨ j₁}
                  (λ j₂ →
                     primPOr {ℓ} (~ j₁) j₁ {λ _ → A} (λ _ → y)
                     (λ _ →
                        hcomp {ℓ} {A} {~ j₂ ∨ j₂ ∨ ~ i'}
                        (λ k →
                           primPOr {ℓ} (~ j₂) (j₂ ∨ ~ i') {λ _ → A} (λ _ → r (~ i'))
                           (primPOr {ℓ} j₂ (~ i') {λ _ → A} (λ _ → s k) (λ _ → s (j₂ ∧ k))))
                        (r (j₂ ∨ ~ i'))))
                  (hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i'}
                   (λ k →
                      primPOr {ℓ} (~ j₁) (j₁ ∨ i') {λ _ → A} (λ _ → y)
                      (primPOr {ℓ} j₁ i' {λ _ → A} (λ _ → r (~ i' ∧ k)) (λ _ → q j₁)))
                   (q j₁))))
            (p j))
         (λ _ →
            hcomp {ℓ} {A} {~ j ∨ j}
            (λ j₁ → primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → s j₁))
            (hcomp {ℓ} {A} {~ j ∨ j}
             (λ j₁ →
                primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
                (λ _ →
                   hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i'}
                   (λ k →
                      primPOr {ℓ} (~ j₁) (j₁ ∨ i') {λ _ → A} (λ _ → q i')
                      (primPOr {ℓ} j₁ i' {λ _ → A} (λ _ → r k) (λ _ → r (j₁ ∧ k))))
                   (q (j₁ ∨ i'))))
             (hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i'}
              (λ k →
                 primPOr {ℓ} (~ j) (j ∨ ~ i') {λ _ → A} (λ _ → x)
                 (primPOr {ℓ} j (~ i') {λ _ → A} (λ _ → q (i' ∧ k)) (λ _ → p j)))
              (p j))))
         ) (primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → v))|])
  ]

runAbstrTests = (putStr . (showAllTestRes . runParserTests (abstr partial0)))
                      abstrParsingTests


exprParsingTests =
  let c0 = foldl addDimToContext emptyC ["j","k","kk","l","i"]
      c = foldl (\cc -> addTyToContext cc (Var 0 []) ) c0
            [ "p", "A", "r", "ℓ", "q","s","w", "x","y","z","w","v"] 
  in
  [
    ok (c ,"p")
  , bad (c ,"primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → v)")
  , bad (c , [r|hcomp {ℓ} {A} {~ j ∨ j ∨ k}
      (primPOr {ℓ} (~ j) (j ∨ k) {λ _ → A} (λ _ → q k)
         (primPOr {ℓ} j k {λ _ → A} (λ _ → r k) (λ _ → r (j ∧ k))))
      (q (j ∨ k))|])
  , ok (c , [r|hcomp {ℓ} {A} {~ j ∨ j ∨ k}
      (λ k₁ → primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x ) (λ _ → v))
      (q (j ∨ k))|])
  , ok (c , [r|hcomp {ℓ} {A} {~ j ∨ j ∨ k}
      (λ k₁ →
         primPOr {ℓ} (~ j) (j ∨ k) {λ _ → A} (λ _ → q k)
         (primPOr {ℓ} j k {λ _ → A} (λ _ → r k₁) (λ _ → r (j ∧ k₁))))
      (q (j ∨ k))|])
  , ok (c , [r|
    hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i}
     (λ k →
        primPOr {ℓ} (~ j) (j ∨ ~ i) {λ _ → A} (λ _ → x)
        (primPOr {ℓ} j (~ i) {λ _ → A}
         (λ _ →
            hcomp {ℓ} {A} {(~ i ∨ ~ k) ∨ i ∧ k}
            (λ j₁ →
               primPOr {ℓ} (~ i ∨ ~ k) (i ∧ k) {λ _ → A} (λ _ → y) (λ _ → r j₁))
            (q (i ∧ k)))
         (λ _ → p j)))
     (p j)|])
    , ok (c , [r|
    hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i}
     (λ k →
        primPOr {ℓ} (~ j) (j ∨ ~ i) {λ _ → A} (λ _ → x)
        (primPOr {ℓ} j (~ i) {λ _ → A}
         (λ _ →
            hcomp {ℓ} {A} {(~ i ∨ ~ k) ∨ i ∧ k}
            (λ j₁ →
               primPOr {ℓ} (~ i ∨ ~ k) (i ∧ k) {λ _ → A} (λ _ → y) (λ _ → r j₁))
            (q (i ∧ k)))
         (λ _ → p j)))
     (p j)|])
    , ok (c , [r|
    hcomp {ℓ} {A} {~ j ∨ j}
    (λ j₁ →
       primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
       (λ _ →
          hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i}
          (λ k →
             primPOr {ℓ} (~ j₁) (j₁ ∨ i) {λ _ → A}
             (λ _ →
                hcomp {ℓ} {A} {~ i ∨ i}
                (λ j₂ → primPOr {ℓ} (~ i) i {λ _ → A} (λ _ → y) (λ _ → r j₂))
                (q i))
             (primPOr {ℓ} j₁ i {λ _ → A} (λ _ → s k) (λ _ → s (j₁ ∧ k))))
          (hcomp {ℓ} {A} {(~ j₁ ∧ ~ i) ∨ j₁ ∨ i}
           (λ j₂ →
              primPOr {ℓ} (~ j₁ ∧ ~ i) (j₁ ∨ i) {λ _ → A} (λ _ → y) (λ _ → r j₂))
           (q (j₁ ∨ i)))))
    (hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i}
     (λ k →
        primPOr {ℓ} (~ j) (j ∨ ~ i) {λ _ → A} (λ _ → x)
        (primPOr {ℓ} j (~ i) {λ _ → A}
         (λ _ →
            hcomp {ℓ} {A} {(~ i ∨ ~ k) ∨ i ∧ k}
            (λ j₁ →
               primPOr {ℓ} (~ i ∨ ~ k) (i ∧ k) {λ _ → A} (λ _ → y) (λ _ → r j₁))
            (q (i ∧ k)))
         (λ _ → p j)))
     (p j))|])
    , ok (c , pent)
  ]

runExprTests = (putStr . (showAllTestRes . runParserTests expr))
                      exprParsingTests

runAllTests =
   putStr $ showRes $ filter (not . fst) $
   (onlyTestRest (runParserTests iExpr iExprParsingTests)
   ++
   onlyTestRest (runParserTests var varParsingTests)
   ++
   onlyTestRest (runParserTests partial partialParsingTests)
   ++
   onlyTestRest (runParserTests (abstr partial0) abstrParsingTests)
   ++
   onlyTestRest (runParserTests expr exprParsingTests)
   )

   
