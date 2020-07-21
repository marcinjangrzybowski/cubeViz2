{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax where


import Data.List

import qualified Data.Tuple.Extra as DTE

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Data.List.Split

-- data IExpr =
--    Min IExpr IExpr |
--    Max IExpr IExpr |
--    Dim Int |
--    Neg IExpr | End Bool
--    deriving (Show , Eq , Ord)


type IExpr = Set.Set ( Set.Set (Int , Bool))

type SubFace = Map.Map Int Bool

type FExpr = Set.Set SubFace


-- minMB :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
-- minMB Nothing _ = Nothing 
-- minMB _ Nothing = Nothing
-- minMB (Just b1) (Just b2) = if b1 == b2 then Just b1 else Nothing


makeAntiH :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set a)
makeAntiH y = Set.filter (\x -> not (any (flip Set.isProperSubsetOf x) y)) y

min :: IExpr -> IExpr -> IExpr
min e1 e2 = makeAntiH $ Set.map (uncurry $ Set.union) $ Set.cartesianProduct e1 e2 

max :: IExpr -> IExpr -> IExpr
max e1 e2 =
  let y1 = Set.filter (\x -> not (any (flip Set.isSubsetOf x) e2)) e1
      y2 = Set.filter (\x -> not (any (flip Set.isProperSubsetOf x) e1)) y1

  in Set.union e1 e2

dim :: Int -> IExpr
dim x = Set.singleton (Set.singleton (x , True))

neg :: IExpr -> IExpr
neg x =
  case Set.maxView x of
    Nothing -> end True
    Just (y , z) ->
      case (Set.maxView y , Set.maxView z) of
        (Nothing , Nothing) -> end False
        (Just _ , Nothing) -> Set.map (Set.singleton . (second not)) y  
        (Nothing , Just _) -> neg z
        (Just _ , Just _) ->
           foldr Syntax.min (end True) $ Set.map (neg . Set.singleton) x

end :: Bool -> IExpr
end True = Set.singleton (Set.empty)
end False = Set.empty

iFromL :: [[(Int, Bool)]] -> IExpr
iFromL = Set.fromList . (map Set.fromList)

-- mmbi :: (Int , (Maybe Bool)) -> [IExpr]
-- mmbi (i , Nothing) = [ Dim i , Neg $ Dim i ]
-- mmbi (i , (Just b)) = [(if b then id else Neg) $ Dim i]

-- fromN :: IExprN -> IExpr
-- fromN = fromL . Set.elems 
--   where
--     fromL [] = End False
--     fromL (x : xs) = foldr (Max . toMin . Map.toList) (toMin . Map.toList $ x) xs
    
--     toMin :: [(Int , (Maybe Bool))] -> IExpr
--     toMin x = h1 $ concatMap mmbi x 
--        where
--          h1 :: [IExpr] -> IExpr
--          h1 [] = End True
--          h1 (y : ys) = foldr Min y ys
    

toSubFace :: IExpr -> FExpr
toSubFace =
   Set.map (Map.fromList . Set.toList . (uncurry Set.union))
   . (Set.filter ((Set.null . (uncurry Set.intersection) . (bimap (Set.map fst) (Set.map fst))) ))
   . (Set.map (Set.partition snd))

type Partial = Map.Map SubFace Expr

partialEmpty :: Partial
partialEmpty = Map.empty

partialConst :: IExpr -> Expr -> Partial
partialConst i e = Map.fromSet (\_ -> e) (toSubFace i)
  
primPOr :: IExpr -> IExpr -> IExpr -> Partial -> Partial -> Either String Partial
primPOr ei ei1 ei2 pa1 pa2 =
  if (ei == Syntax.max ei1 ei2)
  then Right $ Map.union pa1 pa2
  else Left $ "primPOr err - " ++ "\n\n" ++ show ei
                               ++ "\n\n" ++ show ei1
                               ++ "\n\n" ++ show ei2
                               ++ "\n\n" ++ (show $ Syntax.max ei1 ei2) 

data VarIndex = VarIndex Int
  deriving (Eq , Show)

varIndex :: Context -> Int -> Maybe VarIndex
varIndex (Context vs _) i =
  if (i < length vs)
  then (Just (VarIndex i))
  else Nothing

data Expr =
  HComp Name Partial Expr
  | Var VarIndex [IExpr]
  | ILam String Expr
  deriving (Eq , Show)

data BType = BType Int
  deriving (Eq , Show)

data CType =
  CType BType [(Expr , Expr)]
  deriving (Eq , Show)


-- TODO : dodac array leveli
-- TODO : dodac array primitive typow
-- TODO : dodac liste equations, w nich kodowac "brzegi" kolejnych typow

type Name = String

data Env = Env [Name] [(Name , Int)]
   deriving Eq

unsfDefTy :: CType
unsfDefTy = CType (BType 0) []

data Context = Context [(String,CType)] [(String , Maybe Bool)] -- (Maybe CType)
                -- deriving Show

addLevelToEnv :: Env -> Name -> Env
addLevelToEnv (Env ls bts) name = Env (name : ls) bts

addBTyToEnv :: Env -> Name -> Int -> Env
addBTyToEnv (Env ls bts) name (bTyId) = Env ls ((name , bTyId) : bts) 
  
addVarToContext :: Context -> CType -> String -> Context
addVarToContext (Context t d) tm name = Context ((name , tm) : t) d

addDimToContext :: Context -> String -> Context
addDimToContext (Context t d) name = Context t ((name , Nothing) : d)

lookupLevel :: Env -> String -> Maybe Int
lookupLevel (Env ls _) x = ((length ls - 1)-) <$> elemIndex x (ls)

lookupBType :: Env -> String -> Maybe BType
lookupBType (Env  _ dts) x = (BType . ((length dts - 1)-)) <$> elemIndex x (map fst dts) 

lookupDim :: Context -> String -> Maybe Int
lookupDim (Context _ l) x = ((length l - 1)-) <$> elemIndex x (map fst l)

indexS :: Int -> [ a ] -> Maybe a
indexS k l = if (k < length l) then Just (l !! k) else Nothing

getDimSymbol :: Context -> Int -> Either String String
getDimSymbol (Context _ l) i =
   maybe (Left "bad dim abstraction!") (Right . fst)
         (indexS ((length l - 1)- i) l)

lookupVar :: Context -> String -> Maybe Int
lookupVar (Context l _) x = ((length l - 1)-) <$> elemIndex x (map fst l) 

getVarSymbol :: Context -> Int -> Either String String
getVarSymbol (Context l _) i =
  maybe (Left $ "bad var abstraction! : " ++ (show i))
        (Right . fst) (indexS ((length l - 1) - i) l)

getVarType :: Context -> VarIndex -> CType
getVarType (Context l _) (VarIndex i) =
  maybe undefined id (snd <$> (indexS ((length l - 1) - i) l))


instance Show Context where
   show (Context ty dims) =
     "CTypes:\n" ++ (intercalate "\n" (map (\(nm, val) -> nm ++ " : " ++ show val) ty)) 
     ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))

instance Show Env where
   show (Env lvls bTypes) =
     "\nBTypes:\n" ++ (intercalate "\n" (map (\(nm, val) -> nm ++ " : " ++ show val) bTypes)) 
     -- ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))


type Boundary = [((Int,Bool),Expr)]


indent :: Int -> String -> String
indent i s = intercalate ("\n" ++ (replicate i ' ')) (splitOn "\n" s) 

class Codelike a where
  toCode :: Context -> a -> Either String String

  toString :: Context -> a -> String
  toString c = either id id . toCode c
  
instance Codelike (Int , Bool) where
  toCode ctx (i , b) =
    do s <- (getDimSymbol ctx i)
       return ((if b then "" else "~ ")  ++ s)
  
instance Codelike IExpr where
  toCode ctx ie =
    case Set.toList (Set.map Set.toList ie) of
      [] -> return "i0"
      [[]] -> return "i1"
      xs -> do l <- traverse (traverse (toCode ctx)) xs
               return (intercalate " ∨ " (map (intercalate " ∧ ") l))

parr :: String -> String
parr x = "(" ++ x ++ ")" 

instance Codelike Expr where
  toCode c (HComp v pa e) =
     do x <- (toCode (addDimToContext c v) pa)
        y <- (toCode c e)
        return ("hcomp " ++ "(λ " ++ v ++ " → λ { " ++ (indent 5 ("\n" ++ x)) ++ "})\n" ++ parr y )
  toCode c (ILam s e) =
     do x <- (toCode (addDimToContext c s) e)
        return ("λ " ++ s ++ " → " ++ (indent 5 ("\n" ++ x)) ++ "\n" )
  toCode c (Var (VarIndex h) t) =
     do l <- traverse (toCode c) t
        hc <- getVarSymbol c h
        return (hc ++ " " ++ (intercalate " " (map parr l)))
  
instance Codelike SubFace where
  toCode c f =
    do l <- traverse (toCode c) (Map.toList f)
       return (intercalate " ∧ " l) 
       
instance Codelike Partial where
  toCode c pa =
     do l <- traverse (\(f , e) -> do bo <- toCode c e
                                      fc <- toCode c f
                                      return ( (parr $ parr fc ++ " = i1") ++ " → " ++ bo ++ "\n")

                       ) (Map.toList pa)
        return (intercalate ";" l)

  
-- ppIExpr :: IExpr -> String
-- ppIExpr =
--      (intercalate " ∨ ")
--    . map ( (\x -> "(" ++ x ++ ")") . intercalate " ∧ " . (map (\(i , b) -> if b then show i else "~" ++ (show i))) . Set.toList)
--    . (Set.toList)

-- (intercalate "AND" . (map ()))
  
-- instance Show Expr where
--    show (HComp p e) = "xx"
--    show (Var i t) = show i ++ " " ++ intercalate " " (map ((\x -> "(" ++ x ++ ")") . ppIExpr) t)



------ safe functions

emptyEnv :: Env
emptyEnv = Env [] []

emptyCtx :: Context
emptyCtx = Context [] []

mkBType :: Env -> Int -> Either String BType
mkBType (Env lN _) bI =
  if (bI < length lN)
  then Right (BType bI)
  else Left "bad level arg"


getBaseType :: Env -> Context -> CType -> BType
getBaseType _ _ (CType bTy _) = bTy

-- getBaseTypeEx :: Env -> Context -> Expr -> BType
-- getBaseTypeEx _ _ e = undefined

getCTyDim :: Env -> Context -> CType -> Int
getCTyDim e c (CType i faces) = length faces


getExprDim :: Env -> Context -> Expr -> Int
getExprDim e c (Var vI tl) = 
    ((\k -> k - (length tl)) . getCTyDim e c) (getVarType c vI)
getExprDim e c (HComp _ _ x) = getExprDim e c x
getExprDim e c (ILam _ x) = 1 + (getExprDim e c x)

mkCType :: Env -> Context -> (BType , [(Expr , Expr)]) -> Either String CType
mkCType e c (bTy , []) = Right (CType bTy []) 
mkCType e c (bTy , faces) =
  let ged = getExprDim e c in
  if ([(length faces - 1 , length faces - 1) ] == nub (map (bimap ged ged ) $ faces ))
  then (Right (CType bTy faces))
  else (Left "faces of wrong dimension")


mkHcomp ::  Env -> Context -> (String , Partial , Expr) -> Either String Expr
mkHcomp env c (s , pa , e) =
  if (getExprDim env c e == 0)
  then Right (HComp s pa e)
  else Left "only 0-dimensional terms are allowed in center of hcomp"

