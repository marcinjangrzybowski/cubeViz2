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
  ["Goal:"
  ,"Elaborates to:"
  ,"———— Boundary ——————————————————————————————————————————————"
  ,"————————————————————————————————————————————————————————————"
  ,"———— Constraints ———————————————————————————————————————————"
  ]


rawSections :: String -> Either String (String,String,String,String,String)
rawSections x =
  case (splitsBy sectionsHeads x) of
    Right [_ , s1 , s2 , s3 , s4 , s5] -> Right (s1 , s2 , s3 , s4 , s5)
    Right _ -> Left "fatal error in rawSections"
    Left y -> Left ("section: " ++ y ++ " is missing from input")
      

data SessionState = SessionState (Env , Context) [(Expr , Expr)] BType Expr

ssExpr (SessionState _ _ _ e) = e

instance Show SessionState where
   show (SessionState (env , ctx) bndrs bType e) =
     show env ++ "\n"
     ++ show ctx
     ++ "\n"
     ++ "Expr:\n" ++ (toString ctx e)
     ++ "\n"

  
parseInteractive :: String -> Either String SessionState
parseInteractive x =
  do (goalRaw , termRaw , boundaryRaw , contextRaw , constraintsRaw) <- rawSections x
     (env , ctx) <- parseContext contextRaw
     currExpr <- first show $ parseExpr ctx termRaw
     return (SessionState (env , ctx) [] undefined currExpr)




-- assoc0 :: String
-- assoc0 = [r|Term: Context: Goal:|]--zwqContext: qqqq Term: Context:|]
  


-- data StateInputRaw = StateInputRaw {
--       term           :: String
--     , context        :: [(String,String)]
--   } deriving (Show)


-- parseRawInput :: Parser StateInputRaw
-- parseRawInput = return (StateInputRaw "" [])

-- -- line = many $ noneOf "\n"

-- sectionHead =
--     (choice
--        ((map (\s -> try (string (s ++ ":")) )
--        ["Context","Goal","Boundary","Term"] )))

-- section :: Parser (String,String)
-- section =
--   do h <- sectionHead
--      b <- (many (notFollowedBy (sectionHead) >> anyChar))
--      return (h , b)
     
-- mkSectionsParser :: Parser (M.Map String String)
-- mkSectionsParser =
--    (pure M.fromList) <*> (many section)
--    -- sepEndBy
--    --   (sectionHead
--    --            >>= (\x -> return (x , "")))
--    --   (many (notFollowedBy (sectionHead) >> anyChar))

-- parseSctns = parse (mkSectionsParser) ""



-- data Term = Term String
--              deriving Show

-- data Context = Context [(String,Term)] [(String , Maybe Bool)]
--                 -- deriving Show

-- instance Show Context where
--    show (Context ty dims) =
--      "Types:\n" ++ (intercalate "\n" (map (\(nm, Term val) -> nm ++ " : " ++ val) ty)) 
--      ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))

-- type Goal = Term


-- type Boundary = [((Int,Bool),Term)]

-- data Problem = Problem { pContext :: Context
--                        , pGoal :: Goal
--                        , pBoundary :: Boundary
--                        , pTerm :: Maybe Expr }
--                 -- deriving Show

-- instance Show Problem where
--    show (Problem ctx gl bd tm) =
--        "Context:\n\n"
--        ++
--        (show  ctx)
--        ++"\nTerm:\n\n"
--        ++(show tm)

-- ctxDefName =
--     do optional (many space)
--        name <- manyTill (digit <|> letter <|> (char '.')) space
--        manyTill space (char ':')
--        return name

-- addTyToContext :: Context -> String -> Term -> Context
-- addTyToContext (Context t d) name tm = Context ((name , tm) : t) d

-- addDimToContext :: Context -> String -> Context
-- addDimToContext (Context t d) name = Context t ((name , Nothing) : d)
                                       
-- parseCtxDef :: PC.Parsec String Context ()
-- parseCtxDef =
--     do ctx <- getState
--        -- PC.putState (Context (("x", Term "y") : a) b)
--        name <- ctxDefName
--        content <- many space >> many (notFollowedBy (newline >> ctxDefName) >> anyChar)
--        let ty = parseTerm ctx content
--        -- PC.putState (Contex
--        case (ty , content) of
--            (Left _ , _)-> fail ("error while parsing type of " ++ name)
--            (Right t , "I") ->
--              do PC.putState (addDimToContext ctx name)
--                 return ()
--            (Right t , _) ->
--              do PC.putState (addTyToContext ctx name t)
--                 return ()

       
--     -- do name <- (many _)
--     --    return (Context defs [])

-- parseCtxSection :: PC.Parsec String Context Context
-- parseCtxSection =
--     do defs <- (many parseCtxDef)
--        ctx <- getState
--        return ctx

-- parseBoundarySection :: Context -> Parser Boundary
-- parseBoundarySection c = return []

-- termParser :: PC.Parsec String Context Term
-- termParser =
--    do t <- many anyChar
--       return (Term t)

-- oms = optional (many space)

-- discardImplicit = do (between (oms >> (char '{')) (oms >> char '}')
--                         (((try partialPSubE)

--                              <|> (try ((try exprParser) >> (return ())))
--                              <|> (try ((try iExprP)  >> (return ()))))))
--                      oms
--                      return ()

-- mbBetween :: PC.Parsec String Context a -> PC.Parsec String Context a 
-- mbBetween p = (try (between (oms >> (char '(')) (oms >> char ')') p)) <|> (try p)


-- tableI = [    [   E.Prefix ( try (oms >> string "~" >> oms) >> (return Neg))]
--            ,  [ E.Infix ( try (oms >> string "∧" >> oms) >> (return Min)) E.AssocRight]
--            ,  [ E.Infix ( try (oms >> string "∨" >> oms) >> (return Max)) E.AssocRight]
--             ]



-- agdaName = many1 (digit <|> letter <|> (char '.') <|> (oneOf "₀₁₂₃₄₅₆₇₈₉"))

-- iExprVar = do agdaName
--               return (Dim False 0)

              
-- iExprP :: PC.Parsec String Context IExpr
-- iExprP = E.buildExpressionParser tableI
--           (try (between (oms >> (char '(')) (oms >> char ')') iExprP)
--            <|>
--            (try (iExprVar))) 
--             -- letter , digit , char '~' , char '∨' , char '∧'
--             --         , char '(' , char ')', space
            
-- faceP :: PC.Parsec String Context IExpr
-- faceP =
--   do char '('
--      e <- iExprP
--      oms
--      (try (string "= i0)") <|> (try (string "= i1)")))
--      return (e)

-- partialPCase :: PC.Parsec String Context (IExpr , Expr)
-- partialPCase =
--   do oms
--      agdaName
--      oms
--      iE <- faceP
--      oms
--      char '→'
--      e <- ((try exprParser) <|> partialPSub)
--      return (iE , e)

-- partialPSubCase :: PC.Parsec String Context (IExpr , Expr)
-- partialPSubCase =
--   do oms
--      iE <- faceP
--      oms
--      char '→'
--      e <- exprParser
--      return (iE , e)

-- partialP :: PC.Parsec String Context Partial
-- partialP = do oms
--               string "λ {"
--               oms
--               cs <- sepBy partialPCase (char ';')
--               oms
--               char '}'
--               return cs
              
-- partialPSub :: PC.Parsec String Context Expr
-- partialPSub =
--   do oms
--      string "(λ {"
--      oms
--      cs <- sepBy partialPSubCase (char ';')
--      oms
--      string "}) _"
--      oms
--      return (Var "p" 0 [])

-- partialPSubE :: PC.Parsec String Context ()
-- partialPSubE =
--   do oms
--      string "λ {"
--      oms
--      cs <- sepBy partialPSubCase (char ';')
--      oms
--      string "}"
--      oms
--      return ()


-- hcompP :: PC.Parsec String Context Expr
-- hcompP = do oms
--             string "hcomp"
--             discardImplicit
--             discardImplicit
--             discardImplicit
--             si <- (mbBetween partialP)
--             oms
--             lid <- (mbBetween exprParser)
--             return (HComp si lid)



-- varP :: PC.Parsec String Context Expr
-- varP = do
--           -- oms
--           h <- agdaName
--           -- space
--           oms
--           tail <- sepEndBy (iExprP) oms
--           -- tail <- sepBy (iExprP) (try space)
--           return (Var h 0 [])

-- outInP :: PC.Parsec String Context Expr 
-- outInP =
--    do oms
--       string "outS"
--       discardImplicit
--       discardImplicit
--       discardImplicit
--       discardImplicit
--       oms
--       char '('
--       oms
--       string "inS"
--       discardImplicit
--       discardImplicit
--       discardImplicit
--       oms
--       x <- exprParser
--       oms
--       char ')'
--       oms
--       return x
  
-- exprParser :: PC.Parsec String Context Expr
-- exprParser = oms >>
--              (
--               (try (mbBetween hcompP))
--               <|>
--               (try (outInP))
--               <|>
--               (try (mbBetween varP)) 

--              )
                  


-- parseExpr :: Context -> String -> Either ParseError Expr
-- parseExpr c s = runParser (
--                 do x <- exprParser  
--                    eof
--                    return x) c "" s

-- parseTerm :: Context -> String -> Either ParseError Term
-- parseTerm c s = runParser termParser c "" s 

-- -- liftRightMaybe :: Either a (Maybe b) -> Maybe (Either a b)
-- -- liftRightMaybe (Right Nothing) = Nothing
-- -- liftRightMaybe (Right (Just x)) = Just $ Right x
-- -- liftRightMaybe (Left x) = Just $ Left x
-- -- {-# INLINE liftRightMaybe #-}

-- unliftRightMaybe :: Maybe (Either a b) -> Either a (Maybe b)
-- unliftRightMaybe Nothing = (Right Nothing)
-- unliftRightMaybe (Just (Right x)) = (Right (Just x)) 
-- unliftRightMaybe (Just (Left x)) = (Left x) 

-- eitherFlat :: Either a (Either a b) -> Either a b
-- eitherFlat = either Left (either Left Right)

  
-- -- parseProblem :: String -> Either _ _
-- parseProblem rawProblem =
--   do scns <- parseSctns rawProblem
--      ctx <- runParser parseCtxSection (Context [] [])  "" (fromMaybe "" (M.lookup "Context:" scns))
--      gl <- (maybe (Left (
--                           newErrorMessage
--                          (Message "No Goal section present in input!")
--                           (initialPos "")
--                         )) id
--                     $ (fmap (parseTerm ctx) (M.lookup "Term:" scns)))
--      bnd <- parse (parseBoundarySection ctx) (fromMaybe "" (M.lookup "Boundary:" scns)) ""
--      tm <- unliftRightMaybe (fmap (parseExpr ctx) (M.lookup "Term:" scns))
--      return (Problem ctx gl bnd tm)
     
     
     

-- main :: IO ()
-- main = putStrLn (show (parseProblem assoc))
