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
import Data.Function
import Data.List
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
          -> (Int -> Address -> (FromLI Face (Cub b a)) -> a -> Either e aa)
                 -> Address
                 -> Cub b a 
                 -> Either e (Cub bb aa)
cubMap g f addr (Cub fcs a) =
   do let n = (getDim fcs)
      cell <- f n addr fcs a
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

cubMapMayReplace  :: (Int -> Address -> Cub b a -> Maybe (Either e (Cub b a)))
                 -> Address
                 -> Cub b a 
                 -> Either e (Cub b a)
cubMapMayReplace f addr x =
   let n = getDim x
   in case f n addr x of
        Nothing ->
           case x of
              (Hcomp b name pa a) ->
                 do a2 <- cubMapMayReplace f (fullSF (getDim a) : addr) a
                    sides <- traverseMap
                      (\sf -> cubMapMayReplace f (sf : addr))
                        $ pa
                    return (Hcomp b name sides a2) 
              _ -> Right x
        Just y -> y
           

-- variant of cubMap constant on node data
cubMapOld = cubMap (const $ const $ \b -> const $ const $ const $ Right b)
                  

cubMapFill :: (Int -> Address -> (FromLI Face (Cub b a)) -> a -> Either e aa)
                -> ( (Name , (Map.Map SubFace (Cub b a)) , (Cub b a)) -> Map.Map SubFace (Cub b aa)  )
                 -> Address
                 -> Cub b a 
                 -> Either e (Cub b aa)
cubMapFill f g addr (Cub fcs a) =
   do let n = (getDim fcs)
      cell <- f n addr fcs a --
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

class FromCub env a b c where
  fromCub :: env -> Cub b c -> a 



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
          Set.map (SubFace (n - 1) . Map.fromList . Set.toList)
        $ makeAntiH
        $ (Set.map (Set.fromList .
                mapMaybe (\(ii , bb) -> (flip (,) bb) <$> punchOut i ii
                         )
                . Set.toList )        
        $ (Set.map (Set.fromList . Map.toList . (\(SubFace sfN sm) -> sm))
            (Set.filter (\(SubFace sfN sm) -> Map.lookup i sm /= Just (not b)) (Map.keysSet pa))))

    sidesFace :: Map.Map SubFace (Cub b a)
    sidesFace = 
      Map.fromSet
      (\sf@(SubFace sfN sm0) ->
         
         let sm1 = Map.mapKeys (punchIn i) sm0
             smMid = sm1
             smEnd = Map.insert i b sm1
         
         in case (Map.lookup (SubFace (sfN + 1) smMid) pa , Map.lookup (SubFace (sfN + 1) smEnd) pa) of
             (Just _ , Just _) ->
                (error $ "imposible! partial with comparable subfaces!! "
                           ++ show (SubFace (sfN + 1) smMid) ++ " "
                           ++ show (SubFace (sfN + 1) smEnd) ++ " ")
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
       let ct2 = addDimToContext ct (Just nam)
           dim = getDim ee
           pa2 = Map.mapKeys (SubFace dim . Map.mapKeys (toDimI ct2)) $
                 Map.mapWithKey (\sf -> toCub . ((env , addSFConstraintToContext sf ct2),)) pa
           b   = (toCub ((env , ct) , e))
       in (Hcomp () nam pa2 b)

  toCub (ee@(env , ct) , tm@(Var vI tl)) =
     let cell = mkCellExpr ee vI tl
         fcss = fromLIppK
                  (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
                   (exprFaces ct tm )
     in Cub fcss (Right cell)  
  -- toCub ee@((env , ct) , (ILam nam x)) = toCub (first (second (flip addDimToContext nam)) ee)

  toCub ee@((env , ct) , (Hole hI)) =
     let fcss = fromLIppK
                  (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
                  (FromLI (getDim ct) $ const (Hole hI)) 
     in Cub fcss  (Left hI) 

instance FromCub (Env , Context) Expr () (Either Int CellExpr) where
  fromCub ee (Cub _ a) =
    case a of
      Left k -> Hole k
      Right ce -> fromCellExpr ee ce 
  fromCub ee@(env , ct) (Hcomp () nam sides x) =
       let ct2 = addDimToContext ct (Just nam)
           dim = getDim ee
           pa2 = Map.mapWithKey (\sf2 -> fromCub (env , addSFConstraintToContext sf2 ct2))
                 $ Map.mapKeys ((Map.mapKeys (fromDimI ct2)) . (\(SubFace _ sf2) -> sf2) )
                 $ sides
           
    in HComp nam pa2 (fromCub ee x)
    
-- -- -- UNTESTED IN ANY WAY
-- -- makeGrid :: Int -> Int -> Cub ()     
-- -- makeGrid dim 0 = Cub 0 ()
-- -- makeGrid dim depth =
-- --   let prev = (makeGrid dim (depth - 1))
-- --   in Hcomp "z" (Map.fromList $ map (flip (,) prev) (map toSubFace (genAllLI dim)) ) prev 


cubPick :: Address -> Cub b a -> Maybe (Cub b a)
cubPick [] c = Just c
cubPick (_ : _) (Cub _ a) =  Nothing
cubPick addr (Hcomp _ _ si a) =
  case reverse addr of
    sf@(SubFace n ma) : xs ->
      if (Map.null ma)
      then cubPick (reverse xs) a 
      else cubPick (reverse xs) (si Map.! sf)
    _ -> error "imposible"


contextAt :: Context -> Address -> Cub b a -> Context
contextAt ctx [] _ = ctx
contextAt ctx (_ : _) (Cub _ _) = error "unable to dig into cell!" 
contextAt ctx (x : xs) (Hcomp b nam pa a)
   | isFullSF x = contextAt ctx xs a 
   | otherwise = case Map.lookup x pa of
                    Nothing -> error "bad address"
                    Just y -> let c2 = addDimToContext ctx (Just nam)
                                     & addSFConstraintToContext
                                         (((Map.mapKeys (fromDimI ctx)) . (\(SubFace _ sf2) -> sf2)) x )
                              in contextAt c2 xs y
     
data Direction = DParent | DChild | DNext | DPrev

data ImposibleMove = ImposibleMove


cubNav :: Cub b a -> Address -> Direction -> Either ImposibleMove Address

cubNav c addr dir =
  case (cubPick addr c , addr , dir) of
    (Nothing , _ , _) -> error "bad address!"

    (Just _ , (x : xs) , DParent ) -> Right xs
    (Just _ , [] , DParent ) -> Left ImposibleMove
                -- top level cell do not have parent!

    (Just (Cub _ _) , _ , DChild ) -> Left ImposibleMove
                -- leaf do not have childern!
    (Just (Hcomp _ _ _ a) , _ , DChild ) -> Right (fullSF (getDim a) : addr)
                -- leaf do not have childern!                                     

    
    (Just _ , [] , DNext ) -> Left ImposibleMove
    (Just _ , [] , DPrev ) -> Left ImposibleMove
                -- top level cell do not have a parent, so it is not posible to cycle thru its children
                                
    (Just _ , x : xs , nv ) ->
       case cubPick xs c of         
         Just (Hcomp _ _ pa a) -> 
             let
                 sfcs = fullSF (getDim a) : (Map.keys pa)
                 x2 =
                   case nv of
                     DNext -> rotateFrom x sfcs 
                     DPrev -> rotateFrom x (reverse sfcs)
                     _ -> error "imposible"
             in Right (x2 : xs)
         _ -> error "bad address, parent cell is leaf!!"


vaccantSubFaces :: Cub a b -> Address -> Set.Set SubFace
vaccantSubFaces cub addr =
  case (cubPick addr cub) of
    Just (Hcomp _ _ pa a) -> missingSubFaces (getDim a) (Map.keysSet pa)
    _ -> Set.empty
