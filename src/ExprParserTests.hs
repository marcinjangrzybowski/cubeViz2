{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ExprParserTests where

import Text.RawString.QQ

import Text.Parsec
import Text.Parsec.Expr

import Data.List
import Data.Bool
import Data.Either

import Syntax

import ExprParser

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

pentLHS :: String
pentLHS = [r|   hcomp {ℓ} {A} {(~ i ∨ i) ∨ ~ j ∨ j}
    (λ i' →
       primPOr {ℓ} (~ i ∨ i) (~ j ∨ j) {λ _ → A}
       (
          primPOr {ℓ} (~ i) i {λ _ → A}
          (λ _ →
             hcomp {ℓ} {A} {~ j ∨ j}
             (λ j₁ →
                primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
                (λ _ →
                   hcomp {ℓ} {A} {~ j₁ ∨ j₁}
                   (λ j₂ →
                      primPOr {ℓ} (~ j₁) j₁ {λ _ → A} (λ _ → y)
                      (λ _ →
                         hcomp {ℓ} {A} {~ j₂ ∨ j₂}
                         (λ j₃ → primPOr {ℓ} (~ j₂) j₂ {λ _ → A} (λ _ → z) (λ _ → s j₃))
                         (r j₂)))
                   (q j₁)))
             (p j))
          (λ _ →
             hcomp {ℓ} {A} {~ j ∨ j}
             (λ j₁ →
                primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
                (λ _ →
                   hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i'}
                   (λ k →
                      primPOr {ℓ} (~ j₁) (j₁ ∨ i') {λ _ → A} (λ _ → r i')
                      (primPOr {ℓ} j₁ i' {λ _ → A} (λ _ → s k) (λ _ → s (j₁ ∧ k))))
                   (r (j₁ ∨ i'))))
             (hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i'}
              (λ k →
                 primPOr {ℓ} (~ j) (j ∨ ~ i') {λ _ → A} (λ _ → x)
                 (primPOr {ℓ} j (~ i') {λ _ → A} (λ _ → r (i' ∧ k))
                  (λ _ →
                     hcomp {ℓ} {A} {~ j ∨ j}
                     (λ j₁ → primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → q j₁))
                     (p j))))
              (hcomp {ℓ} {A} {~ j ∨ j}
               (λ j₁ → primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → q j₁))
               (p j))))
          )
       (primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → v)))
    (hcomp {ℓ} {A} {~ j ∨ j}
     (λ j₁ →
        primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x)
        (λ _ →
           hcomp {ℓ} {A} {~ j₁ ∨ j₁ ∨ i}
           (λ k →
              primPOr {ℓ} (~ j₁) (j₁ ∨ i) {λ _ → A} (λ _ → q i)
              (primPOr {ℓ} j₁ i {λ _ → A}
               (λ _ →
                  hcomp {ℓ} {A} {~ k ∨ k}
                  (λ j₂ → primPOr {ℓ} (~ k) k {λ _ → A} (λ _ → z) (λ _ → s j₂))
                  (r k))
               (λ _ →
                  hcomp {ℓ} {A} {(~ j₁ ∨ ~ k) ∨ j₁ ∧ k}
                  (λ j₂ →
                     primPOr {ℓ} (~ j₁ ∨ ~ k) (j₁ ∧ k) {λ _ → A} (λ _ → z) (λ _ → s j₂))
                  (r (j₁ ∧ k)))))
           (q (j₁ ∨ i))))
     (hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i}
      (λ k →
         primPOr {ℓ} (~ j) (j ∨ ~ i) {λ _ → A} (λ _ → x)
         (primPOr {ℓ} j (~ i) {λ _ → A} (λ _ → q (i ∧ k)) (λ _ → p j)))
      (p j)))|]
  
pentRHS :: String
pentRHS = [r|hcomp {ℓ} {A} {(~ i ∨ i) ∨ ~ j ∨ j}
               (λ i' →
                  primPOr {ℓ} (~ i ∨ i) (~ j ∨ j) {λ _ → A}
                  (primPOr {ℓ} (~ i) i {λ _ → A}
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
                                     (primPOr {ℓ} j₂ (~ i') {λ _ → A} (λ _ → s k)
                                      (λ _ → s (j₂ ∧ k))))
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
                        (p j)))))
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


--tests:

runParserTests :: Codelike b => Parsec String a b
                   -> [(a , Bool , String)]
                   -> [((a , Bool , String) , Either (ParseError) b)]
runParserTests p = map (\(x , b , y) -> ((x , b , y) , (runParser p x "" y)))


onlyTestRest :: Codelike b => [((Context , Bool , String) , Either (ParseError) b)]
                   -> [(Bool , (String , String ))]
onlyTestRest = map (\((c , b , y) , e) -> ( b == (isRight e) , (y , (either show (either id id . toCode c)) e)))

showAllTestRes :: Codelike b => [((Context , Bool , String) , Either (ParseError) b)]
                   -> String
showAllTestRes =  

   ( ++ "\n") . intercalate "\n\n" . (map (\((c , b , y) , rslt ) ->
                                
                              let info = either
                                        (\z -> y ++ "\n" ++ show z)
                                        (\z -> y  ++ "\n output: \n" ++ (either id id $ (toCode c) z))
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
  let c = foldl addDimToContext emptyCtx ["j","k","l","i"]
  in
  [ok (c , "j")
  ,ok (c , "(j)")
  ,ok (c , "j ∨ k")
  ,ok (c , "j ∧ k ∧ l ")
  ,ok (c , "j ∨ k   ∧ l ")
  ,ok (c , "j ∨ k  ∨ l")
  ,ok (c , "j ∧    k ∨ ~ l")
  ,ok (c , "( j ∨ ( j     ∨ (i ∧ ~ (~       k) )))")
  ,ok (c , "~ (~ (( j ∨ ~ (i ∧ k))))")
  ,ok (c , "~ j ∧ j ∨ ~ k")]


runIExprTests = (putStr . (showAllTestRes . runParserTests iExpr)) iExprParsingTests

varParsingTests =
  let c0 = foldl addDimToContext emptyCtx ["j","k","l","i"]
      c = foldl (\cc -> addVarToContext cc (unsfDefTy) ) c0
            [ "p", "A", "r", "ℓ", "q", "x","y","z","w","v"] 
  in
  [ok (c , "p")
  ,ok (c , "p j")
  ,ok (c , "p (j)")
  ,ok (c , "q (j ∨ k) i")
  ,bad (c , "q j ∨ k (~ i)")
  ,bad (c , "q (j ∨ k) ~ i")
  ,ok (c , "q (j ∨ k) (~ i)")
  ]


runVarTests = (putStr . (showAllTestRes . runParserTests var)) varParsingTests



partialParsingTests =
  let c0 = foldl addDimToContext emptyCtx ["j","k","kk","l","i"]
      c = foldl (\cc -> addVarToContext cc (unsfDefTy) ) c0
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

-- runPartialTests = (putStr . (showAllTestRes . runParserTests partial))
--                       partialParsingTests

abstrParsingTests =
  let c0 = foldl addDimToContext emptyCtx ["j","k","kk","l","i"]
      c = foldl (\cc -> addVarToContext cc (undefined) ) c0
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

-- runAbstrTests = (putStr . (showAllTestRes . runParserTests (abstr partial0)))
--                       abstrParsingTests


exprParsingTests =
  let c0 = foldl addDimToContext emptyCtx ["j","k","kk","l","i"]
      c = foldl (\cc -> addVarToContext cc (undefined) ) c0
            [ "p", "A", "r", "ℓ", "q","s","w", "x","y","z","w","v"] 
  in
  [
    ok (c ,"p ")
  , ok (c ,"p i")
  , ok (c ,"p i j ")
  , ok (c ,"p i (i ∨ j) ")
  , ok (c ,"p (i ∨ j) i")
  , bad (c ,"primPOr {ℓ} (~ j) j {λ _ → A} (λ _ → x) (λ _ → v)")
  , bad (c , [r|hcomp {ℓ} {A} {~ j ∨ j ∨ k}
      (primPOr {ℓ} (~ j) (j ∨ k) {λ _ → A} (λ _ → q k)
         (primPOr {ℓ} j k {λ _ → A} (λ _ → r k) (λ _ → r (j ∧ k))))
      (q (j ∨ k))|])
  , ok (c , [r|hcomp {ℓ} {A} {~ j ∨ j ∨ k}
      (λ k₁ → primPOr {ℓ} (~ j) (j ∨ k) {λ _ → A} (λ _ → x ) (λ _ → v))
      (q (j ∨ k)) |])
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
            (q (i ∧ k) ) )
         (λ _ → p j)))
     (p j) |])
    , bad (c , [r|
    hcomp {ℓ} {A} {~ j ∨ j ∨ ~ i}
     (λ k →
        primPOr {ℓ} (~ i) (j ∨ ~ i) {λ _ → A} (λ _ → x)
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
    , ok (c , pentRHS)
    , ok (c , pentLHS)
  ]

runExprTests = (putStr . (showAllTestRes . runParserTests expr))
                      exprParsingTests

runAllTests =
   putStr $ showRes $ filter (not . fst) $
   (onlyTestRest (runParserTests iExpr iExprParsingTests)
   ++
   onlyTestRest (runParserTests var varParsingTests)
   ++
   -- onlyTestRest (runParserTests partial partialParsingTests)
   -- ++
   -- onlyTestRest (runParserTests (abstr partial0) abstrParsingTests)
   -- ++
   onlyTestRest (runParserTests expr exprParsingTests)
   )

   


iExprNormalFormTests :: [(String , String)]
iExprNormalFormTests =
  [  ("i ∨ j ∨ k" , "k ∨ j ∨ i")
   , ("(((i ∨ j) ∨ k) ∨ l)" , "(i ∨ (j ∨ (k ∨ l)))")
   , ("i ∧ j ∧ k" , "k ∧ j ∧ i")
   , ("(((i ∧ j) ∧ k) ∧ l)" , "(i ∧ (j ∧ (k ∧ l)))")
    ]
