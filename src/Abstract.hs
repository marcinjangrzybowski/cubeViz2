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
import Data.Functor

import Control.Applicative

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

data Cub b a =
    Cub (FromLI Face (Cub b a)) a
  | Hcomp b Name (Map.Map SubFace (Cub b a)) (Cub b a)
  deriving (Show ,  Functor)


instance Bifunctor Cub where
  first f (Cub fc a) = (Cub (fmap (first f) fc) a) 
  first f (Hcomp b nm si a) = (Hcomp (f b) nm (fmap (first f) si) (first f a))

  second = fmap

type Address = [SubFace]

instance OfDim (Cub b a) where
  getDim (Cub fc _) = getDim fc 
  getDim (Hcomp b nm pa a) = getDim a

     
cubMap :: (Int -> Address -> b -> Name -> (Map.Map SubFace (Cub b a)) -> (Cub b a) -> Either e bb)
          -> (Int -> Address -> a -> Either e aa)
                 -> Address
                 -> Cub b a 
                 -> Either e (Cub bb aa)
cubMap g f addr (Cub fcs a) =
   do let n = (getDim fcs)
      cell <- f n addr a
      fcs2 <- traverseMapLI (\fc -> cubMap g f (toSubFace fc : addr)) $ fcs
      return $ Cub fcs2 cell

cubMap g f addr (Hcomp b name pa a) = 
   do
      let n = (getDim a)
      bot <- cubMap g f (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMap g f (sf : addr))
                 $ pa
      b2 <- g n addr b name pa a
      return (Hcomp b2 name sides bot) 

-- variant of cubMap constant on node data
cubMapOld = cubMap (const $ const $ \b -> const $ const $ const $ Right b)

cubMapFill :: (Int -> Address -> a -> Either e aa)
                -> ( (Name , (Map.Map SubFace (Cub b a)) , (Cub b a)) -> Map.Map SubFace (Cub b aa)  )
                 -> Address
                 -> Cub b a 
                 -> Either e (Cub b aa)
cubMapFill f g addr (Cub fcs a) =
   do let n = (getDim fcs)
      cell <- f n addr a --
      fcs2 <- traverseMapLI (\fc -> cubMapOld f (toSubFace fc : addr)) $ fcs
      return $ Cub fcs2 cell

cubMapFill f g addr (Hcomp b name pa a) = 
   do 
      bot <- cubMapFill f g (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMapFill f g (sf : addr))
                 $ pa
      let sidesFilled = Map.union sides (g (name , pa , a))
      return (Hcomp b name sidesFilled bot) 



foldSubFaces :: (b -> a -> (Map.Map SubFace a) -> a) -> Cub b a -> a
foldSubFaces f (Cub _ a) = a
foldSubFaces f (Hcomp b _ pa a) =
  f b (foldSubFaces f a) $ (fmap (foldSubFaces f) pa)


foldFaces :: (b -> a -> (Map.Map Face a) -> a) -> Cub b a -> a
foldFaces f (Cub _ a) = a
foldFaces f (Hcomp b _ pa a) =
   f b (foldFaces f a) $
     (fmap (foldFaces f) $ Map.fromList $
        mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
      $ Map.toList pa)

-- requires for allFacesToBeFilled!
foldFacesFiled :: (b -> a -> (FromLI Face a) -> a) -> Cub b a -> a
foldFacesFiled f (Cub _ a) = a
foldFacesFiled f cub@(Hcomp b _ pa a) =
   let li = fromMapFLIUnsafe (getDim cub)
                (Map.fromList $ mapMaybe (\(sf , aa) -> (flip (,) (foldFacesFiled f aa)) <$> toFace sf ) $ Map.toList pa) 
       
   in f b (foldFacesFiled f a) li
   
   -- f (foldFaces f a) $
   --   (fmap (foldFaces f) $ Map.fromList $
   --      mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
   --    $ Map.toList pa)
  
class OfDim a => ToCub a b c where
  toCub :: a -> (Cub b c)

class FromCub a b c where
  fromCub :: Cub b c -> a 



-- TODO : extract some code from below and move to Cube
-- sfProj , sfInj

-- WARNING, node data (of b type), are simply copied without any processing!

cubFace :: forall a b. Face -> Cub b a -> Cub b a
cubFace fc (Cub fcs a) = appLI fc fcs
cubFace fc@(Face n (i , b))  cub@(Hcomp bb nam pa a)
    | getDim cub /= n = error "dim of face do not mach dim of cub!!"
    | Map.member (toSubFace fc) pa =

  cubFace (Face n (n - 1 , True)) (pa Map.! (toSubFace fc))


    | otherwise =
                                                    
  Hcomp bb nam
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


    sidesFace :: Map.Map SubFace (Cub b a)
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

    
instance ToCub ((Env , Context) , Expr) () (Either Int CellExpr) where
  toCub ee@((env , ct) , (HComp nam pa e)) =
       let ct2 = addDimToContext ct nam
           dim = getDim ee
           pa2 = Map.mapKeys (SubFace dim . Map.mapKeys (toDimI ct2)) $
                 Map.mapWithKey (\sf -> toCub . ((env , addSFConstraintToContext sf ct2),)) pa
           b   = (toCub ((env , ct) , e))
       in (Hcomp () nam pa2 b)

  toCub (ee@(env , ct) , (Var vI tl)) =
     let (cell , fcs) = mkCellExpr ee vI tl
         fcss = fromLIppK
                  (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
                 fcs
     in Cub fcss (Right cell)  
  toCub ee@((env , ct) , (ILam nam x)) = toCub (first (second (flip addDimToContext nam)) ee)

  toCub ee@((env , ct) , (Hole hI)) =
     let fcss = fromLIppK
                  (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
                  (FromLI (getDim ct) $ const (Hole hI)) 
     in Cub fcss  (Left hI) 


-- -- -- UNTESTED IN ANY WAY
-- -- makeGrid :: Int -> Int -> Cub ()     
-- -- makeGrid dim 0 = Cub 0 ()
-- -- makeGrid dim depth =
-- --   let prev = (makeGrid dim (depth - 1))
-- --   in Hcomp "z" (Map.fromList $ map (flip (,) prev) (map toSubFace (genAllLI dim)) ) prev 
