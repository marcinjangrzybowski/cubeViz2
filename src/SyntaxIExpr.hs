{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SyntaxIExpr where


import Data.List

import qualified Data.Tuple.Extra as DTE

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Data.List.Split
import Data.Maybe
import Data.Bool
import Data.Either
import Data.Function
import qualified Data.List.NonEmpty as Ne

import qualified Text.Read as TR

import DataExtra

import Debug.Trace

import Combi

-- do przemyslenia:
-- moze zrobic werse Conetxtu gdzie nie ma zadncyh dimension?
-- tak zeby nie uzywac Contextu w miejscach gdzie dimension nie maja prawa byc jeszcze zdefiniowane

-- data IExpr =
--    Min IExpr IExpr |
--    Max IExpr IExpr |
--    Dim Int |
--    Neg IExpr | End Bool
--    deriving (Show , Eq , Ord)


-- TODO : parametrise this type by convention of indezes ov variables

data IExpr = IExpr (Set.Set ( Set.Set (Int , Bool)))
  deriving (Eq)

instance Show IExpr where

  show (IExpr x) = show x



data VarIndex = VarIndex Int
  deriving (Eq , Show, Ord)

data DimIndex = DimIndex Int
  deriving (Eq , Show)


remapIExpr :: (Int -> Int) -> IExpr -> IExpr
remapIExpr f (IExpr x) = IExpr (Set.map (Set.map (first f)) x) 

remapIExprDir :: Set.Set Int -> IExpr -> IExpr
remapIExprDir dims (IExpr x) = IExpr (Set.map (Set.map (\(i,b) -> if Set.member i dims then (i , not b) else (i , b) )) x) 



  
setSetElim :: b -> b -> ([[a]] -> b) -> (Set.Set (Set.Set a)) -> b
setSetElim em emSing f s =
  case Set.toList s of
    [] -> em
    x : xs ->
      case Set.toList x of
        [] -> emSing
        _ -> f $ (map Set.toList (x : xs))
        -- if antihereditary property of s is assumed
        -- this branch is guaranted to not contain empty lists

--- this is why IExpr should be implemented via NON EMPTY SETS/MAPS
elimIExpr :: ([[(Int , Bool)]] -> b) -> IExpr -> b
elimIExpr f (IExpr x) =
  (setSetElim (error "wrong IExpr") (error "wrong IExpr") f) x

substIExpr :: (VarIndex -> Maybe IExpr) -> IExpr -> IExpr
substIExpr f x =
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                let a =  case f (VarIndex k) of
                            Nothing -> dim k
                            Just y -> y
                    (IExpr aa) = if b
                               then a
                               else neg a
                 in aa                   
                                    ))) x


  in IExpr (unsafeMaxOf (fmap unsafeMinOf x2)) 

-- slighlty difeent signature
substIExpr' :: [IExpr] -> IExpr -> IExpr
substIExpr' ies x = 
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                let a = ies !! k
                    (IExpr aa) = if b
                                 then a
                                 else neg a
                 in aa ))) x


  in IExpr (unsafeMaxOf (fmap unsafeMinOf x2)) 

  

--this do not refere DimIndexes, but to VarIndexes!
type SubFace2 = Map.Map Int Bool



iExprToSubFace2 :: IExpr -> [SubFace2]
iExprToSubFace2 = 
  let z = elimIExpr $ fmap
             (\x ->
               let (eq1 , eq0) = partition snd x
               in if (null $ intersect (map fst eq0) (map fst eq1))
                  then Just (Map.fromList x)
                  else Nothing
               )
  in catMaybes . z 

-- TODO :: This would be good place to return type of subface which cant be Full
iExprToSubFace :: Int -> IExpr -> [SubFace]
iExprToSubFace n = 
  let z = elimIExpr $ fmap
             (\x ->
               let (eq1 , eq0) = partition snd x
               in if (null $ intersect (map fst eq0) (map fst eq1))
                  then Just $ SubFace n (Map.fromList x)
                  else Nothing
               )
  in catMaybes . z 




subFace2ToIExpr :: SubFace2 -> IExpr
subFace2ToIExpr sf2 =
    mapToSet sf2
  & Set.singleton
  & IExpr

subFaceToIExpr :: SubFace -> IExpr
subFaceToIExpr (SubFace _ sf) =
    mapToSet sf
  & Set.singleton
  & IExpr

  
substSubFace2 :: (VarIndex -> Maybe IExpr) -> SubFace2 -> [SubFace2]
substSubFace2 f = (iExprToSubFace2 . substIExpr f . subFace2ToIExpr)

substSubFace :: Int -> [IExpr] -> SubFace -> [SubFace]
substSubFace n ies sf0 = --(iExprToSubFace n . substIExpr' ies . subFaceToIExpr)
  let ie0 = subFaceToIExpr sf0
      ie1 = substIExpr' ies ie0 
      sfs = iExprToSubFace n ie1
      ies' = (fmap snd $ filter (isNothing . fst) $ safeZip (toListLI sf0) ies)
      subs = filter (\sf' ->
                let ies'' = fmap (projIExpr' sf') ies'
                in all isRight ies''
             ) $ nub (concatMap (allSubSubFaces) sfs)
      
  in sfs ++ subs

projIExpr :: SubFace2 -> IExpr -> Either Bool IExpr
projIExpr sf2 x =
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                  case Map.lookup k sf2 of
                    Just bb -> internalEnd (xor bb (not b))  
                    Nothing -> (if b
                                then (dim k)
                                else (neg (dim k))) & (\(IExpr z) -> z)

                                    
                                    ))) x


  in elimUnsafeIExpr (unsafeMaxOf (fmap unsafeMinOf x2))

projIExpr' :: SubFace -> IExpr -> Either Bool IExpr
projIExpr' (SubFace _ sf) x =
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                  case Map.lookup k sf of
                    Just bb -> internalEnd (xor bb (not b))  
                    Nothing -> (if b
                                then (dim k)
                                else (neg (dim k))) & (\(IExpr z) -> z)

                                    
                                    ))) x

      toPunchOut = Map.keysSet sf
  in elimUnsafeIExpr (unsafeMaxOf (fmap unsafeMinOf x2))
     & second (remapIExpr (fromJust . punchOutMany toPunchOut ))



type FExpr = Set.Set SubFace2

type Face2 = (Int , Bool)

-- codim ?
-- sfDim :: SubFace2 -> Int
-- sfDim = length . Map.keys

faceToFace2 :: Face -> Face2
faceToFace2 (Face n f) = f
  
toFace2 :: SubFace2 -> Maybe Face2
toFace2 sf =
  case (Map.toList sf) of
    [ x ] -> Just x
    _ -> Nothing

face2ToSubFace2 :: Face2 -> SubFace2
face2ToSubFace2 x = Map.fromList [x]

-- minMB :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
-- minMB Nothing _ = Nothing 
-- minMB _ Nothing = Nothing
-- minMB (Just b1) (Just b2) = if b1 == b2 then Just b1 else Nothing



 
unsafeMin e1 e2 =  (makeAntiH $ Set.map (uncurry $ Set.union) $ Set.cartesianProduct e1 e2) 


unsafeMax e1 e2 =
  let y1 = Set.filter (\x -> not (any (flip Set.isSubsetOf x) e2)) e1
      y2 = Set.filter (\x -> not (any (flip Set.isProperSubsetOf x) e1)) y1

  in (Set.union e1 e2)


unsafeMaxOf [] = error "arg of unsafeMaxOf cannot be empty!"
unsafeMaxOf (x : xs) = foldl unsafeMax x xs

unsafeMinOf [] = error "arg of unsafeMinOf cannot be empty!"
unsafeMinOf (x : xs) = foldl unsafeMin x xs

min :: IExpr -> IExpr -> IExpr
min (IExpr e1) (IExpr e2) = IExpr (unsafeMin e1 e2) 

max :: IExpr -> IExpr -> IExpr
max (IExpr e1) (IExpr e2) = IExpr (unsafeMax e1 e2)

dim :: Int -> IExpr
dim x = IExpr (Set.singleton (Set.singleton (x , True)))

internalEnd True = Set.singleton (Set.empty)
internalEnd False = Set.empty


unsafeEnd = error "attempt to create IExpr represetig i0 or i1"
-- end True = Set.singleton (Set.empty)
-- end False = Set.empty


unsafeNeg x = 
  (case Set.maxView x of
    Nothing -> unsafeEnd True
    Just (y , z) ->
      case (Set.maxView y , Set.maxView z) of
        (Nothing , Nothing) -> unsafeEnd False
        (Just _ , Nothing) -> Set.map (Set.singleton . (second not)) y  
        (Nothing , Just _) -> unsafeNeg z
        (Just _ , Just _) ->
           foldr SyntaxIExpr.unsafeMin ((unsafeNeg . Set.singleton) y) $ Set.map (unsafeNeg . Set.singleton) z)

neg (IExpr x) = IExpr (unsafeNeg x)

elimUnsafeIExpr :: (Set.Set ( Set.Set (Int , Bool))) -> Either Bool IExpr
elimUnsafeIExpr = setSetElim (Left False) (Left True) (Right . IExpr . Set.fromList . fmap (Set.fromList))  


end :: Bool -> IExpr
end = error "attempt to create IExpr represetig i0 or i1"
-- end True = Set.singleton (Set.empty)
-- end False = Set.empty

endView :: IExpr -> Maybe Bool
endView x | x == end True = Just True
          | x == end False = Just False
          | otherwise = Nothing  
          
iFromL :: [[(Int, Bool)]] -> IExpr
iFromL [] = error "attempt to create IExpr represetig i0"
iFromL ([[]]) = error "attempt to create IExpr represetig i1"
iFromL x = IExpr $ (Set.fromList . (map Set.fromList)) x


-- getIExprFace :: Face -> Context -> IExpr -> IExpr
-- getIExprFace (Face _ (i0 , b)) ctx =
--    setSetElim (end False) (end True) (makeAntiH . Set.fromList . (fmap Set.fromList) . f)
  
--   where
--     i :: Int
--     i = fromDimI ctx i0

--     substHelp :: (Int , Bool) -> Maybe (Maybe (Int , Bool))
--     substHelp (j , bb) | j /= i = Just (Just (j , bb))
--                       | bb == b = Nothing -- term evaluates to one, can be removed from /\
--                       | otherwise = Just Nothing -- we note term evaluating to zero, and later discared whole /\
                      
--     f :: [[(Int , Bool)]] -> [[(Int , Bool)]]
--     f l = 
--       let l0 = map (mapMaybe substHelp) l
--           l1 = filter (all isJust) l0
--       in map (map fromJust) l1
    
-- -- mmbi :: (Int , (Maybe Bool)) -> [IExpr]
-- -- mmbi (i , Nothing) = [ Dim i , Neg $ Dim i ]
-- -- mmbi (i , (Just b)) = [(if b then id else Neg) $ Dim i]

-- -- fromN :: IExprN -> IExpr
-- -- fromN = fromL . Set.elems 
-- --   where
-- --     fromL [] = End False
-- --     fromL (x : xs) = foldr (Max . toMin . Map.toList) (toMin . Map.toList $ x) xs
    
-- --     toMin :: [(Int , (Maybe Bool))] -> IExpr
-- --     toMin x = h1 $ concatMap mmbi x 
-- --        where
-- --          h1 :: [IExpr] -> IExpr
-- --          h1 [] = End True
-- --          h1 (y : ys) = foldr Min y ys
    

toSubFace2 :: IExpr -> FExpr
toSubFace2 (IExpr x) =
   (Set.map (Map.fromList . Set.toList . (uncurry Set.union))
   . (Set.filter ((Set.null . (uncurry Set.intersection) . (bimap (Set.map fst) (Set.map fst))) ))
   . (Set.map (Set.partition snd))) x
