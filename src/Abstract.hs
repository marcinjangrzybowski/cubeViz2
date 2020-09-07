{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Abstract where

import Syntax

-- import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable

import Control.Applicative

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

data Cub a = Cub Int (FromLI Face (Cub a)) a | Hcomp Name (Map.Map SubFace (Cub a)) (Cub a)
  deriving (Show , Functor)


type Address = [SubFace]



instance OfDim (Cub a) where
  getDim (Cub n _ _) = n 
  getDim (Hcomp nm pa x) = getDim x

     
cubMap :: (Int -> Address -> a -> Either e b)
                 -> Address
                 -> Cub a 
                 -> Either e (Cub b)
cubMap f addr (Cub n fcs a) =
   do cell <- f n addr a
      fcs2 <- traverseMapLI (\fc -> cubMap f (toSubFace fc : addr)) $ fcs
      return $ Cub n fcs2 cell

cubMap f addr (Hcomp name pa a) = 
   do 
      bot <- cubMap f (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMap f (sf : addr))
                 $ pa
      return (Hcomp name sides bot) 


cubMapFill :: (Int -> Address -> a -> Either e b)
                -> ( (Name , (Map.Map SubFace (Cub a)) , (Cub a)) -> Map.Map SubFace (Cub b)  )
                 -> Address
                 -> Cub a 
                 -> Either e (Cub b)
cubMapFill f g addr (Cub n fcs a) =
   do cell <- f n addr a
      fcs2 <- traverseMapLI (\fc -> cubMap f (toSubFace fc : addr)) $ fcs
      return $ Cub n fcs2 cell

cubMapFill f g addr (Hcomp name pa a) = 
   do 
      bot <- cubMapFill f g (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMapFill f g (sf : addr))
                 $ pa
      let sidesFilled = Map.union sides (g (name , pa , a))
      return (Hcomp name sidesFilled bot) 



foldSubFaces :: (a -> (Map.Map SubFace a) -> a) -> Cub a -> a
foldSubFaces f (Cub _ _ a) = a
foldSubFaces f (Hcomp _ pa a) =
  f (foldSubFaces f a) $ (fmap (foldSubFaces f) pa)


foldFaces :: (a -> (Map.Map Face a) -> a) -> Cub a -> a
foldFaces f (Cub _ _ a) = a
foldFaces f (Hcomp _ pa a) =
   f (foldFaces f a) $
     (fmap (foldFaces f) $ Map.fromList $
        mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
      $ Map.toList pa)

-- requires for allFacesToBeFilled!
foldFacesFiled :: (a -> (FromLI Face a) -> a) -> Cub a -> a
foldFacesFiled f (Cub _ _ a) = a
foldFacesFiled f cub@(Hcomp _ pa a) =
   let li = fromMapFLIUnsafe (getDim cub)
                (Map.fromList $ mapMaybe (\(sf , aa) -> (flip (,) (foldFacesFiled f aa)) <$> toFace sf ) $ Map.toList pa) 
       
   in f (foldFacesFiled f a) li
   
   -- f (foldFaces f a) $
   --   (fmap (foldFaces f) $ Map.fromList $
   --      mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
   --    $ Map.toList pa)
  
class OfDim a => ToCub a c where
  toCub :: a -> (Cub c)

class FromCub a c where
  fromCub :: Cub c -> a 



-- TODO : extract some code from below and move to Cube
-- sfProj , sfInj

cubFace :: forall a. Face -> Cub a -> Cub a
cubFace fc (Cub n fcs a) = appLI fc fcs
cubFace fc@(Face n (i , b))  cub@(Hcomp nam pa a) | getDim cub /= n = error "dim of face do not mach dim of cub!!"
                                                  | Map.member (toSubFace fc) pa =

  cubFace (Face n (n - 1 , True)) (pa Map.! (toSubFace fc))


                                                  | otherwise =
                                                    
  Hcomp nam
   sidesFace
   (cubFace fc a)

  where

    subfaces =
      (Set.map (SubFace (n - 1) . Map.fromList .
                mapMaybe (\(ii , bb) -> (flip (,) bb) <$> punchOut i ii
                         )
                . Set.toList )
        $ makeAntiH2 $
         (Set.map (Set.fromList . Map.toList . (\(SubFace sfN sm) -> sm))
            (Set.filter (\(SubFace sfN sm) -> Map.lookup i sm /= Just (not b)) (Map.keysSet pa))))


    sidesFace :: Map.Map SubFace (Cub a)
    sidesFace =
      Map.fromSet
      (\sf@(SubFace sfN sm0) ->

         
         let sm1 = Map.mapKeys (punchIn i) sm0
             smMid = sm1
             smEnd = Map.insert i b sm1
         
         in case (Map.lookup (SubFace (sfN + 1) smMid) pa , Map.lookup (SubFace (sfN + 1) smEnd) pa) of
             (Just _ , Just _) -> error "imposible!"
             (Nothing , Nothing) -> error "imposible!"
             (Just x , _) ->
                    cubFace
                    (Face (n - subFaceCodim sf + 1)  (i - (Set.size (Set.filter (\j -> j < i) $ Map.keysSet sm0) )  , b))
                    x
             (_ , Just x) -> x
      )
      subfaces

    
instance ToCub ((Env , Context) , Expr) CellExpr where
  toCub ee@((env , ct) , (HComp nam pa e)) =
       let ct2 = addDimToContext ct nam
           dim = getDim ee
           pa2 = Map.mapKeys (SubFace dim . Map.mapKeys (toDimI ct2)) $
                 Map.mapWithKey (\sf -> toCub . ((env , addSFConstraintToContext sf ct2),)) pa
           b   = (toCub ((env , ct) , e))
       in (Hcomp nam pa2 b)

  toCub (ee@(env , ct) , (Var vI tl)) =
     let (cell , fcs) = mkCellExpr ee vI tl
         fcss = fromLIppK
                  (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
                 fcs
     in Cub (getDim ee) fcss cell  
  toCub ee@((env , ct) , (ILam nam x)) = toCub (first (second (flip addDimToContext nam)) ee) 


-- -- UNTESTED IN ANY WAY
-- makeGrid :: Int -> Int -> Cub ()
-- makeGrid dim 0 = Cub 0 ()
-- makeGrid dim depth =
--   let prev = (makeGrid dim (depth - 1))
--   in Hcomp "z" (Map.fromList $ map (flip (,) prev) (map toSubFace (genAllLI dim)) ) prev 
