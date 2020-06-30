{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Text.RawString.QQ
import Data.List
import qualified Data.Map as M
import qualified Data.Either as E
import Data.Maybe


import Data.Bifunctor
import Data.Either.Utils

import qualified Text.Parsec as PC
import qualified Text.Parsec.Expr as E
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Prim as PP
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Syntax

assoc :: String
assoc = [r|Context:
j     : I
i     : I
s     : w ≡ v
r     : z ≡ w
q     : y ≡ z
p     : x ≡ y
v     : x.A   (not in scope)
w     : x.A   (not in scope)
z     : x.A   (not in scope)
y     : x.A
x     : x.A
x.A   : Type x.A.ℓ   (not in scope)
x.A.ℓ : Level   (not in scope)

Goal:
x.A

Boundary:
k = i0 ⊢ hcomp {x.A.ℓ} {x.A} {~ i ∨ i}
         (λ { j (i = i0) → x
            ; j (i = i1)
                → hcomp {x.A.ℓ} {x.A} {~ j ∨ j}
                  (Cubical.Foundations.Prelude..extendedlambda0 {x.A.ℓ} {x.A} {y} {y}
                   {z} {w} (λ _ → y) q r j)
                  (q j)
            })
         (p i)
k = i1 ⊢ hcomp {x.A.ℓ} {x.A} {~ i ∨ i}
         (λ { j (i = i0) → x ; j (i = i1) → r j })
         (hcomp {x.A.ℓ} {x.A} {~ i ∨ i}
          (λ { j (i = i0) → x ; j (i = i1) → q j }) (p i))
i = i0 ⊢ x
i = i1 ⊢ w

Term:
hcomp {x.A.ℓ} {x.A} {(~ i ∨ i) ∨ ~ j ∨ j}
(λ i' →
   primPOr {x.A.ℓ} (~ i ∨ i) (~ j ∨ j) {λ _ → x.A}
   (λ .o →
      primPOr {x.A.ℓ} (~ i) i {λ _ → _≡_ {x.A.ℓ} {x.A} x v}
      (λ _ i₁ →
         hcomp {x.A.ℓ} {x.A} {~ i₁ ∨ i₁}
         (λ j₁ →
            primPOr {x.A.ℓ} (~ i₁) i₁ {λ _ → x.A} (λ _ → x)
            (λ _ →
               hcomp {x.A.ℓ} {x.A} {~ j₁ ∨ j₁}
               (λ j₂ →
                  primPOr {x.A.ℓ} (~ j₁) j₁ {λ _ → x.A} (λ _ → y)
                  (λ _ →
                     hcomp {x.A.ℓ} {x.A} {~ j₂ ∨ j₂ ∨ ~ i'}
                     (λ k →
                        primPOr {x.A.ℓ} (~ j₂) (j₂ ∨ ~ i') {λ _ → x.A} (λ _ → r (~ i'))
                        (primPOr {x.A.ℓ} j₂ (~ i') {λ _ → x.A} (λ _ → s k)
                         (λ _ → s (j₂ ∧ k))))
                     (r (j₂ ∨ ~ i'))))
               (hcomp {x.A.ℓ} {x.A} {~ j₁ ∨ j₁ ∨ i'}
                (λ k →
                   primPOr {x.A.ℓ} (~ j₁) (j₁ ∨ i') {λ _ → x.A} (λ _ → y)
                   (primPOr {x.A.ℓ} j₁ i' {λ _ → x.A} (λ _ → r (~ i' ∧ k))
                    (λ _ → q j₁)))
                (q j₁))))
         (p i₁))
      (λ _ i₁ →
         hcomp {x.A.ℓ} {x.A} {~ i₁ ∨ i₁}
         (λ j₁ →
            primPOr {x.A.ℓ} (~ i₁) i₁ {λ _ → x.A} (λ _ → x) (λ _ → s j₁))
         (hcomp {x.A.ℓ} {x.A} {~ i₁ ∨ i₁}
          (λ j₁ →
             primPOr {x.A.ℓ} (~ i₁) i₁ {λ _ → x.A} (λ _ → x)
             (λ _ →
                hcomp {x.A.ℓ} {x.A} {~ j₁ ∨ j₁ ∨ i'}
                (λ k →
                   primPOr {x.A.ℓ} (~ j₁) (j₁ ∨ i') {λ _ → x.A} (λ _ → q i')
                   (primPOr {x.A.ℓ} j₁ i' {λ _ → x.A} (λ _ → r k) (λ _ → r (j₁ ∧ k))))
                (q (j₁ ∨ i'))))
          (hcomp {x.A.ℓ} {x.A} {~ i₁ ∨ i₁ ∨ ~ i'}
           (λ k →
              primPOr {x.A.ℓ} (~ i₁) (i₁ ∨ ~ i') {λ _ → x.A} (λ _ → x)
              (primPOr {x.A.ℓ} i₁ (~ i') {λ _ → x.A} (λ _ → q (i' ∧ k))
               (λ _ → p i₁)))
           (p i₁))))
      _ j)
   (primPOr {x.A.ℓ} (~ j) j {λ _ → x.A} (λ _ → x) (λ _ → v)))
(hcomp {x.A.ℓ} {x.A} {~ j ∨ j}
 (λ j₁ →
    primPOr {x.A.ℓ} (~ j) j {λ _ → x.A} (λ _ → x)
    (λ _ →
       hcomp {x.A.ℓ} {x.A} {~ j₁ ∨ j₁ ∨ i}
       (λ k →
          primPOr {x.A.ℓ} (~ j₁) (j₁ ∨ i) {λ _ → x.A}
          (λ _ →
             hcomp {x.A.ℓ} {x.A} {~ i ∨ i}
             (λ j₂ → primPOr {x.A.ℓ} (~ i) i {λ _ → x.A} (λ _ → y) (λ _ → r j₂))
             (q i))
          (primPOr {x.A.ℓ} j₁ i {λ _ → x.A} (λ _ → s k) (λ _ → s (j₁ ∧ k))))
       (hcomp {x.A.ℓ} {x.A} {(~ j₁ ∧ ~ i) ∨ j₁ ∨ i}
        (λ j₂ →
           primPOr {x.A.ℓ} (~ j₁ ∧ ~ i) (j₁ ∨ i) {λ _ → x.A} (λ _ → y)
           (λ _ → r j₂))
        (q (j₁ ∨ i)))))
 (hcomp {x.A.ℓ} {x.A} {~ j ∨ j ∨ ~ i}
  (λ k →
     primPOr {x.A.ℓ} (~ j) (j ∨ ~ i) {λ _ → x.A} (λ _ → x)
     (primPOr {x.A.ℓ} j (~ i) {λ _ → x.A}
      (λ _ →
         hcomp {x.A.ℓ} {x.A} {(~ i ∨ ~ k) ∨ i ∧ k}
         (λ j₁ →
            primPOr {x.A.ℓ} (~ i ∨ ~ k) (i ∧ k) {λ _ → x.A} (λ _ → y)
            (λ _ → r j₁))
         (q (i ∧ k)))
      (λ _ → p j)))
  (p j)))|]



assoc0 :: String
assoc0 = [r|Term: Context: Goal:|]--zwqContext: qqqq Term: Context:|]
  


data StateInputRaw = StateInputRaw {
      term           :: String
    , context        :: [(String,String)]
  } deriving (Show)


parseRawInput :: Parser StateInputRaw
parseRawInput = return (StateInputRaw "" [])

-- line = many $ noneOf "\n"

sectionHead =
    (choice
       ((map (\s -> try (string (s ++ ":")) )
       ["Context","Goal","Boundary","Term"] )))

section :: Parser (String,String)
section =
  do h <- sectionHead
     b <- (many (notFollowedBy (sectionHead) >> anyChar))
     return (h , b)
     
mkSectionsParser :: Parser (M.Map String String)
mkSectionsParser =
   (pure M.fromList) <*> (many section)
   -- sepEndBy
   --   (sectionHead
   --            >>= (\x -> return (x , "")))
   --   (many (notFollowedBy (sectionHead) >> anyChar))

parseSctns = parse (mkSectionsParser) ""



-- data Term = Term String
--              deriving Show

type Type = Expr

data Context = Context [(String,Type)] [(String , Maybe Bool)]
                -- deriving Show

instance Show Context where
   show (Context ty dims) =
     "Types:\n" ++ (intercalate "\n" (map (\(nm, val) -> nm ++ " : " ++ show val) ty)) 
     ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))

type Goal = Expr


type Boundary = [((Int,Bool),Expr)]

data Problem = Problem { pContext :: Context
                       , pGoal :: Goal
                       , pBoundary :: Boundary
                       , pTerm :: Maybe Expr }
                -- deriving Show

instance Show Problem where
   show (Problem ctx gl bd tm) =
       "Context:\n\n"
       ++
       (show  ctx)
       ++"\nTerm:\n\n"
       ++(show tm)

-- ctxDefName =
--     do optional (many space)
--        name <- manyTill (digit <|> letter <|> (char '.')) space
--        manyTill space (char ':')
--        return name

-- addTyToContext :: Context -> String -> Expr -> Context
-- addTyToContext (Context t d) name tm = Context ((name , tm) : t) d

-- addDimToContext :: Context -> String -> Context
-- addDimToContext (Context t d) name = Context t ((name , Nothing) : d)
                                       
parseCtxDef :: PC.Parsec String Context ()
parseCtxDef = do (return ())
    -- do ctx <- ge-- tState
       -- -- PC.putState (Context (("x", Term "y") : a) b)
       -- name <- ctxDefName
       -- content <- many space >> many (notFollowedBy (newline >> ctxDefName) >> anyChar)
       -- let ty = parseTerm ctx content
       -- -- PC.putState (Contex
       -- case (ty , content) of
       --     (Left _ , _)-> fail ("error while parsing type of " ++ name)
       --     (Right t , "I") ->
       --       do PC.putState (addDimToContext ctx name)
       --          return ()
       --     (Right t , _) ->
             -- do PC.putState (addTyToContext ctx name t)
       --          return ()

       
    -- do name <- (many _)
    --    return (Context defs [])

parseCtxSection :: PC.Parsec String Context Context
parseCtxSection =
    do defs <- (many parseCtxDef)
       ctx <- getState
       return ctx

parseBoundarySection :: Context -> Parser Boundary
parseBoundarySection c = return []


parseExpr :: Context -> String -> Either ParseError Expr
parseExpr c s = Right (Var "" 0 [])

unliftRightMaybe :: Maybe (Either a b) -> Either a (Maybe b)
unliftRightMaybe Nothing = (Right Nothing)
unliftRightMaybe (Just (Right x)) = (Right (Just x)) 
unliftRightMaybe (Just (Left x)) = (Left x) 

eitherFlat :: Either a (Either a b) -> Either a b
eitherFlat = either Left (either Left Right)

  
-- parseProblem :: String -> Either _ _
parseProblem rawProblem =
  do scns <- parseSctns rawProblem
     ctx <- runParser parseCtxSection (Context [] [])  "" (fromMaybe "" (M.lookup "Context:" scns))
     gl <- (maybe (Left (
                          newErrorMessage
                         (Message "No Goal section present in input!")
                          (initialPos "")
                        )) id
                    $ (fmap (parseExpr ctx) (M.lookup "Term:" scns)))
     bnd <- parse (parseBoundarySection ctx) (fromMaybe "" (M.lookup "Boundary:" scns)) ""
     tm <- unliftRightMaybe (fmap (parseExpr ctx) (M.lookup "Term:" scns))
     return (Problem ctx gl bnd tm)
     
     
     

main :: IO ()
main = putStrLn (show (parseProblem assoc))


