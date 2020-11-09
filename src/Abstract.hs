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

import Data.Functor.Identity

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

data ClCub b = ClCub {clCub :: (FromLI SubFace (OCub b))}
  deriving (Show ,  Functor)

data CylCub b = CylCub {cylCub :: (FromLI SubFace (Maybe (OCub b)))}
  deriving (Show ,  Functor)

data OCub b =
    Cub Int b (Maybe CellExpr)
  | Hcomp b (Maybe Name) (CylCub b) (ClCub b)
  deriving (Show ,  Functor)


-- instance Bifunctor Cub where
--   first f (Cub fc a) = (Cub (fmap (first f) fc) a) 
--   first f (Hcomp b nm si a) = (Hcomp (f b) nm (fmap (first f) si) (first f a))

--   second = fmap



data AddressPart = AOnCylinder SubFace | AOnBottom SubFace 
  deriving (Show)
data Address = Address SubFace [AddressPart] 
  deriving (Show)

-- type Address = [SubFace]

instance OfDim (ClCub b) where
  getDim (ClCub fli) = getDim fli
  
instance OfDim (OCub b) where
  getDim (Cub n _ _) = n 
  getDim (Hcomp _ _ _ a) = getDim a

instance OfDim Address where
  getDim (Address sf _) = getDim sf 

                           

cubMapTrav ::  forall a b bb. (Monad a) => (Int -> Address -> b -> Maybe Name -> (CylCub b) -> (ClCub b) -> a bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> a bb)                
                 -> ClCub b 
                 -> a (ClCub bb)
cubMapTrav g f (ClCub xx) = fmap ClCub $
   traverseMapLI (\sf -> cm (Address sf [])) xx

  where
     cm :: (Monad a) => Address -> OCub b -> a (OCub bb)
     cm addr@(Address _ tla) (Cub n b mbc) =
          do b2 <- f n addr b mbc
             return $ Cub n b2 mbc

     cm addr@(Address sf0 tla) ocub@(Hcomp b mbNm cyl btm) =
          do b2 <- g (getDim ocub) addr b mbNm cyl btm
             cyl2 <- fmap CylCub
                     $ traverseMapLI
                       (\sf -> maybe (return Nothing) (fmap Just . cm (Address sf0 (AOnCylinder sf : tla))))
                   (cylCub cyl)
             btm2 <- fmap ClCub $ traverseMapLI (\sf -> cm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
             return $ (Hcomp b2 mbNm cyl2 btm2)

cubMap :: (Int -> Address -> b -> Maybe Name -> CylCub b -> ClCub b -> bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> bb)                
                 -> ClCub b 
                 -> (ClCub bb)
cubMap f g = runIdentity . cubMapTrav (dot6 Identity f) (dot4 Identity  g)




cubMapMayReplace  :: forall a b . (Monad a) =>  (Int -> Address -> OCub b -> a (Maybe (OCub b)))
                 -> ClCub b 
                 -> a (ClCub b)
cubMapMayReplace f (ClCub xx) = fmap ClCub $
   traverseMapLI (\sf -> cmm (Address sf [])) xx

  where
    cmm :: (Monad a) =>  Address
                 -> OCub b 
                 -> a (OCub b)
    cmm addr@(Address sf0 tla) x = do
       y <- f (getDim x) addr x
       case y of
         Just y -> return y
         Nothing ->
           case x of
             Hcomp b mbNm cyl btm ->
                do cyl2 <- fmap CylCub
                     $ traverseMapLI
                       (\sf -> maybe (pure Nothing) $ (fmap Just . cmm (Address sf0 (AOnCylinder sf : tla))))
                        (cylCub cyl)
                   btm2 <- fmap ClCub $ traverseMapLI (\sf -> cmm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
                   return (Hcomp b mbNm cyl2 btm2)
                   
             Cub _ _ _ -> return x 


-- -- variant of cubMap constant on node data
cubMapOld = cubMap (const $ const $ \b -> const $ const $ const $ b)
                  


cubMapFill :: forall a b . (Monad a) => (Int -> Address -> (CylCub b , ClCub b) -> a (CylCub b)  )
                 -> ClCub b 
                 -> a (ClCub b)
cubMapFill f (ClCub xx) = fmap ClCub $
   traverseMapLI (\sf -> cmf (Address sf [])) xx

  where
    cmf :: (Monad a) =>  Address
                 -> OCub b 
                 -> a (OCub b)
    cmf addr@(Address sf0 tla) ocub@(Cub _ _ _) = return ocub
    cmf addr@(Address sf0 tla) ocub@(Hcomp b mbNm cyl btm) = do
        cyl2 <- fmap CylCub
                     $ traverseMapLI
                       (\sf -> maybe (pure Nothing) $ (fmap Just . cmf (Address sf0 (AOnCylinder sf : tla))))
                        (cylCub cyl)
        btm2 <- fmap ClCub $ traverseMapLI (\sf -> cmf (Address sf0 (AOnBottom sf : tla))) (clCub btm)
        cylFilled <- f (getDim ocub) addr (cyl2 , btm2)

        return $ Hcomp b mbNm cylFilled btm2

instance Foldable (OCub) where 
  foldMap f (Cub _ b _) = f b 
  foldMap f (Hcomp b _ cyl btm) =  (f b) <> (foldMap f cyl) <> (foldMap f btm)  

instance Foldable (ClCub) where 
  foldMap f (ClCub w) = foldMap (foldMap f) w

instance Foldable (CylCub) where 
  foldMap f (CylCub w) = mconcat $ fmap (foldMap f ) $ catMaybes $ toListFLI w 


-- foldSubFaces :: (b -> a -> (Map.Map SubFace a) -> a) -> Cub b a -> a
-- foldSubFaces f (Cub _ a) = a
-- foldSubFaces f (Hcomp b _ pa a) =
--   f b (foldSubFaces f a) $ (fmap (foldSubFaces f) pa)



-- foldFaces :: (b -> a -> (Map.Map Face a) -> a) -> Cub b a -> a
-- foldFaces f (Cub _ a) = a
-- foldFaces f (Hcomp b _ pa a) =
--    f b (foldFaces f a) $
--      (fmap (foldFaces f) $ Map.fromList $
--         mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
--       $ Map.toList pa)

-- -- requires for allFacesToBeFilled!
-- foldFacesFiled :: (b -> a -> (FromLI Face a) -> a) -> Cub b a -> a
-- foldFacesFiled f (Cub _ a) = a
-- foldFacesFiled f cub@(Hcomp b _ pa a) =
--    let li = fromMapFLIUnsafe (getDim cub)
--                 (Map.fromList $ mapMaybe (\(sf , aa) -> (flip (,) (foldFacesFiled f aa)) <$> toFace sf ) $ Map.toList pa) 
       
--    in f b (foldFacesFiled f a) li
   
--    -- f (foldFaces f a) $
--    --   (fmap (foldFaces f) $ Map.fromList $
--    --      mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
--    --    $ Map.toList pa)
  
-- class OfDim a => ToCub a b where
--   toCub :: a -> (ClCub b c)

-- class FromCub env a b where
--   fromCub :: env -> (ClCub b) -> a 



-- -- TODO : extract some code from below and move to Cube
-- -- sfProj , sfInj

-- -- WARNING, node data (of b type), are simply copied without any processing!


-- injectDim :: Int ->  Cub b a -> Cub b a
-- injectDim = undefined
-- -- injectDim k cub@(Hcomp bb nam pa a) =
-- --   let paNew = Map.mapWithKey
-- --               (\(SubFace sfN sm) -> \si -> undefined)
-- --               pa

-- --   in (Hcomp bb nam paNew (injectDim a))
  
-- -- injectDim k cub@(Cub fcs (Left h)) = Cub fcs (Left h) 
-- -- injectDim k cub@(Cub fcs (Right ce)) = Cub fcs undefined 




clCubFace :: forall b. Face -> ClCub b -> ClCub b
clCubFace fc = ClCub . ccf . clCub 

  where
    ccf (FromLI 0 _) = error "attempt to take face of 0-dimensional cub"
    ccf (FromLI n mp) =
      FromLI (n - 1) (mp . flip injSubFace (toSubFace fc))

clCubSubFace :: forall b. SubFace -> ClCub b -> ClCub b
clCubSubFace sf = ClCub . ccf . clCub 

  where
    ccf (FromLI n mp) =
      FromLI (subFaceDimEmb sf) (mp . flip injSubFace sf)
      
      
-- cubFace :: forall a b. Face -> Cub b a -> Cub b a
-- cubFace fc (Cub fcs a) = appLI fc fcs
-- cubFace fc@(Face n (i , b))  cub@(Hcomp bb nam pa a)
--     | getDim cub /= n = error "dim of face do not mach dim of cub!!"
--     | Map.member (toSubFace fc) pa =

--   cubFace (Face n (n - 1 , True)) (pa Map.! (toSubFace fc))


--     | otherwise =
                                                    
--   Hcomp bb nam
--    sidesFace
--    (cubFace fc a)

--   where

    
--     subfaces =
--           Set.map (SubFace (n - 1) . Map.fromList . Set.toList)
--         $ makeAntiH
--         $ (Set.map (Set.fromList .
--                 mapMaybe (\(ii , bb) -> (flip (,) bb) <$> punchOut i ii
--                          )
--                 . Set.toList )        
--         $ (Set.map (Set.fromList . Map.toList . (\(SubFace sfN sm) -> sm))
--             (Set.filter (\(SubFace sfN sm) -> Map.lookup i sm /= Just (not b)) (Map.keysSet pa))))

--     sidesFace :: Map.Map SubFace (Cub b a)
--     sidesFace = 
--       Map.fromSet
--       (\sf@(SubFace sfN sm0) ->
         
--          let sm1 = Map.mapKeys (punchIn i) sm0
--              smMid = sm1
--              smEnd = Map.insert i b sm1
         
--              ff =
--               case (Map.lookup (SubFace (sfN + 1) smMid) pa , Map.lookup (SubFace (sfN + 1) smEnd) pa) of
--                 (Just _ , Just _) ->
--                    (error $ "imposible! partial with comparable subfaces!! "
--                               ++ show (SubFace (sfN + 1) smMid) ++ " "
--                               ++ show (SubFace (sfN + 1) smEnd) ++ " ")
--                 (Nothing , Nothing) -> error "imposible!"
--                 (Just x , _) ->
--                        cubFace
--                        (Face (n - subFaceCodim sf + 1)
--                              (i - (Set.size (Set.filter (\j -> j < i) $ Map.keysSet sm0) )  , b))
--                        x
--                 (_ , Just x) -> x

--          in ff
--       )
--       subfaces

-- cubHole :: Int -> (Cub () (Either Int a))
-- cubHole n =
--      let fcss = (FromLI n $ const (cubHole (n - 1))) 
--      in Cub fcss (Left 0) 




toClCub :: ((Env , Context) , Expr) -> ClCub ()

toClCub  ee@((env , ct) , expr) =
  ClCub $ fromLIppK
                  (\sfc -> \e -> toOCub ((env , addSFConstraintToContext sfc ct) , e) )
                    (exprSubFaces ct expr )
--    ClCub (FromLI (getDim ct) (\sf -> Cub (subFaceDimEmb sf) () Nothing))

-- toClCub  (ee@(env , ct) , tm@(Var vI tl)) =
--   ClCub (FromLI (getDim ct) (\sf -> Cub (subFaceDimEmb sf) () Nothing))
    
-- toClCub  ee@((env , ct) , (HComp nam pa e)) = undefined



toOCub :: ((Env , Context) , Expr) -> OCub ()

toOCub ee@((env , ct) , (Hole hI)) = Cub (getDim ct) () Nothing 

toOCub (ee@(env , ct) , tm@(Var vI tl)) = Cub (getDim ct) () $
    Just $ mkCellExpr ee vI (fmap snd tl)

 
    
toOCub ee@((env , ct) , (HComp nam pa e)) = 
       let ct2 = addDimToContext ct nam
           dim = getDim ee
           cyl = CylCub $
                 fromMapFLI dim $
                 Map.mapKeys (sf2ToSubFace ct) $                 
                 Map.mapWithKey (\sf2 -> toOCub . ((env , addSF2ConstraintToContext sf2 ct2),)) $
                 partialWithSF ct $
                 pa
                 
           btm = toClCub ((env , ct) , e)
       in (Hcomp () nam cyl btm)


fromCylCub :: (Env , Context) -> CylCub () -> Partial
fromCylCub = undefined --makeAntiHKeys

fromOCub :: (Env , Context) -> OCub () -> Expr

fromOCub ee (Cub _ _ mbCe) = maybe (Hole 0) (fromCellExprSafe ee) $ mbCe 

fromOCub ee (Hcomp b nm cyl btm) =
  HComp nm (fromCylCub ee cyl) (fromOCub ee (appLI (fullSF (getDim ee)) $ clCub btm))
                              
-- instance ToCub ((Env , Context) , Expr) () (Either Int CellExpr) where
--   toCub ee@((env , ct) , (HComp nam pa e)) =
--        let ct2 = addDimToContext ct (Just nam)
--            dim = getDim ee
--            pa2 = Map.mapKeys (SubFace dim . Map.mapKeys (toDimI ct2)) $
--                  Map.mapWithKey (\sf -> toCub . ((env , addSFConstraintToContext sf ct2),)) pa
--            b   = (toCub ((env , ct) , e))
--        in (Hcomp () nam pa2 b)

--   toCub (ee@(env , ct) , tm@(Var vI tl)) =
--      let cell = mkCellExpr ee vI tl
--          fcss = fromLIppK
--                   (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
--                    (exprFaces ct tm )
--      in Cub fcss (Right cell)  
--   -- toCub ee@((env , ct) , (ILam nam x)) = toCub (first (second (flip addDimToContext nam)) ee)

--   toCub ee@((env , ct) , (Hole hI)) = cubHole (getDim ct)

-- instance FromCub (Env , Context) Expr () (Either Int CellExpr) where
--   fromCub ee (Cub _ a) =
--     case a of
--       Left k -> Hole k
--       Right ce -> fromCellExprSafe ee ce 
--   fromCub ee@(env , ct) (Hcomp () nam sides x) =
--        let ct2 = addDimToContext ct (Just nam)
--            dim = getDim ee
--            pa2 = Map.mapWithKey (\sf2 -> fromCub (env , addSFConstraintToContext sf2 ct2))
--                  $ Map.mapKeys ((Map.mapKeys (fromDimI ct2)) . (\(SubFace _ sf2) -> sf2) )
--                  $ sides
           
--     in HComp nam pa2 (fromCub ee x)
    
-- -- -- -- UNTESTED IN ANY WAY
-- -- -- makeGrid :: Int -> Int -> Cub ()     
-- -- -- makeGrid dim 0 = Cub 0 ()
-- -- -- makeGrid dim depth =
-- -- --   let prev = (makeGrid dim (depth - 1))
-- -- --   in Hcomp "z" (Map.fromList $ map (flip (,) prev) (map toSubFace (genAllLI dim)) ) prev 


-- cubPick :: Address -> Cub b a -> Maybe (Cub b a)
-- cubPick [] c = Just c
-- cubPick (_ : _) (Cub _ a) =  Nothing
-- cubPick addr (Hcomp _ _ si a) =
--   case reverse addr of
--     sf@(SubFace n ma) : xs ->
--       if (Map.null ma)
--       then cubPick (reverse xs) a 
--       else cubPick (reverse xs) (si Map.! sf)
--     _ -> error "imposible"



-- contextAt :: Context -> Address -> Cub b a -> Context
-- contextAt ctx [] _ = ctx
-- contextAt ctx (_ : _) (Cub _ _) = error "unable to dig into cell!" 
-- contextAt ctx addr@(_ : _) (Hcomp b nam pa a) = h $ reverse addr
--   where

--    h (x : xs) | isFullSF x = contextAt ctx (reverse xs) a 
--    h (x : xs) | otherwise = case Map.lookup x pa of
--                     Nothing -> error "bad address"
--                     Just y -> let c2 = addDimToContext ctx (Just nam)
--                                      & addSFConstraintToContext
--                                          (((Map.mapKeys (fromDimI ctx)) . (\(SubFace _ sf2) -> sf2)) x )
--                               in contextAt c2 (reverse xs) y
     
-- data Direction = DParent | DChild | DNext | DPrev

-- data ImposibleMove = ImposibleMove


-- negateDimIndexes :: Set.Set Int -> Cub () (Either Int CellExpr) -> Cub () (Either Int CellExpr)
-- negateDimIndexes dims (Cub (FromLI n fcsM) a) =
--   Cub (FromLI n (\fc@(Face k (i , b)) ->
--           negateDimIndexes (setMapMaybe (punchOut i) dims) (fcsM (Face k (i , xor (Set.member i dims) b)))
--                    ))
--    (bimap id (negateCellExprShallow dims) a)
   
-- -- negateDimIndexes dims (Hcomp b nam pa a) =
  
-- --   let n = getDim a
-- --       pa2 = (Map.mapWithKey
-- --              (\(SubFace _ sfm) -> 
-- --                 negateDimIndexes $ (Set.map ( fromMaybe (n - Set.size dims ) . punchOutMany (Map.keysSet sfm)) dims)
-- --              ) pa)
-- --       pa3 = (Map.mapKeys (fromListLI . zipWith (\i -> fmap $ xor (Set.member i dims) ) [0..] . toListLI ) pa2)
-- --   in (Hcomp b nam pa3 (negateDimIndexes dims a))

-- negateDimIndexes dims (Hcomp b nam pa a) =
  
--   let n = getDim a
--       pa2 = (Map.mapWithKey
--              (\(SubFace _ sfm) -> 
--                 negateDimIndexes $ (setMapMaybe (punchOutMany (Map.keysSet sfm)) dims)
--              ) pa)
--       pa3 = (Map.mapKeys (fromListLI . zipWith (\i -> fmap $ xor (Set.member i dims) ) [0..] . toListLI ) pa2)
--   in (Hcomp b nam pa3 (negateDimIndexes dims a))


-- rotateDimIndexes :: (Int , Int) -> Cub () (Either Int CellExpr) -> Cub () (Either Int CellExpr)
-- rotateDimIndexes ( i , j ) =    
--    permuteDimIndexes (Set.fromList [i , j])
--  . negateDimIndexes (Set.fromList [i])

-- permuteDimIndexes :: Set.Set Int -> Cub () (Either Int CellExpr) -> Cub () (Either Int CellExpr)
-- permuteDimIndexes dims cub
--   | Set.size dims <= 1  = cub
--   | otherwise = remapDimIndexes
--        (\k ->
--          if Set.member k dims
--          then rotateFrom k (Set.toList dims)
--          else k) cub


-- -- f must be bijection !!! 
-- remapDimIndexes :: (Int -> Int) -> Cub () (Either Int CellExpr) -> Cub () (Either Int CellExpr)
-- remapDimIndexes f (Cub fcs@(FromLI n fcsM) a) =
--   let fcs2 =
--          (Map.mapWithKey
--              (\(Face _ (i , b)) ->
--            remapDimIndexes $ (
--                    (fromJust . punchOut (f i))
--                  . f
--                  . (punchIn i)
--                  )
--              ) (toMapFLI fcs))
            
--       fcs3 = (Map.mapKeys (\(Face n (i , b)) ->  (Face n (f i , b)) ) fcs3)
--   in Cub
--       (fromMapFLIUnsafe n fcs3)
--        (bimap id (remapCellExprShallow f) a)
   
-- remapDimIndexes f (Hcomp b nam pa a) =
  
--   let n = getDim a
--       pa2 = (Map.mapWithKey
--              (\(SubFace _ sfm) ->
--            remapDimIndexes $ (
--                    (fromJust . punchOutMany (Set.map f $ Map.keysSet sfm))
--                  . f
--                  . (punchInMany (Map.keysSet sfm))
--                  )
--              ) pa)
            
--       pa3 = (Map.mapKeys (\(SubFace n sfm) ->  (SubFace n (Map.mapKeys f sfm)) ) pa2)
--   in (Hcomp b nam pa3 (remapDimIndexes f a))



-- injDim :: Int -> Cub () (Either Int CellExpr) -> Cub () (Either Int CellExpr)
-- injDim k = undefined

-- cubNav :: Cub b a -> Address -> Direction -> Either ImposibleMove Address

-- cubNav c addr dir =
--   case (cubPick addr c , addr , dir) of
--     (Nothing , _ , _) -> error "bad address!"

--     (Just _ , (x : xs) , DParent ) -> Right xs
--     (Just _ , [] , DParent ) -> Left ImposibleMove
--                 -- top level cell do not have parent!

--     (Just (Cub _ _) , _ , DChild ) -> Left ImposibleMove
--                 -- leaf do not have childern!
--     (Just (Hcomp _ _ _ a) , _ , DChild ) -> Right (fullSF (getDim a) : addr)
--                 -- leaf do not have childern!                                     

    
--     (Just _ , [] , DNext ) -> Left ImposibleMove
--     (Just _ , [] , DPrev ) -> Left ImposibleMove
--                 -- top level cell do not have a parent, so it is not posible to cycle thru its children
                                
--     (Just _ , x : xs , nv ) ->
--        case cubPick xs c of         
--          Just (Hcomp _ _ pa a) -> 
--              let
--                  sfcs = fullSF (getDim a) : (Map.keys pa)
--                  x2 =
--                    case nv of
--                      DNext -> rotateFrom x sfcs 
--                      DPrev -> rotateFrom x (reverse sfcs)
--                      _ -> error "imposible"
--              in Right (x2 : xs)
--          _ -> error "bad address, parent cell is leaf!!"


-- vaccantSubFaces :: Cub a b -> Address -> Set.Set SubFace
-- vaccantSubFaces cub addr =
--   case (cubPick addr cub) of
--     Just (Hcomp _ _ pa a) -> missingSubFaces (getDim a) (Map.keysSet pa)
--     _ -> Set.empty
