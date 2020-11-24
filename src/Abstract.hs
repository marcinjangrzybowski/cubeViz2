{-# LANGUAGE FlexibleContexts #-}

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

import Debug.Trace

data ClCub b = ClCub {clCub :: FromLI SubFace (OCub b)}
  deriving (Show ,  Functor)

data CylCub b = CylCub {cylCub :: FromLI SubFace (Maybe (OCub b))}
  deriving (Functor)

--everywhere but on fullSF, this would be place to introduce Boundary LI
data BdCub b = BdCub {bdCub :: FromLI SubFace (OCub b)}
  deriving (Functor)

instance (Show b) => Show (CylCub b) where
  show x = showMbFLI (cylCub x)

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

rootAddress :: Int -> Address
rootAddress n = Address (fullSF n) []

instance OfDim (ClCub b) where
  getDim (ClCub fli) = getDim fli

instance OfDim (OCub b) where
  getDim (Cub n _ _) = n
  getDim (Hcomp _ _ _ a) = getDim a

instance OfDim Address where
  getDim (Address sf _) = getDim sf

bdCubPickSF :: SubFace -> BdCub a -> ClCub a
bdCubPickSF sfc _ | isFullSF sfc = error "you cannot pick full SF from Bd"
bdCubPickSF sfc (BdCub (FromLI n mp)) = ClCub $
   FromLI (n - subFaceCodim sfc)
          (mp . flip injSubFace sfc)

clCubPickSF :: SubFace -> ClCub a -> ClCub a
clCubPickSF sfc (ClCub (FromLI n mp)) = ClCub $
   FromLI (n - subFaceCodim sfc)
          (mp . flip injSubFace sfc)

clInterior :: ClCub a -> OCub a
clInterior (ClCub (FromLI n mp)) = mp (fullSF n)

clBoundary :: ClCub a -> BdCub a
clBoundary x =
    let g sf c | isFullSF sf = error "you cannot ask for interoir of border! - todo special LI for borders..."
        g sf c  = c
    in BdCub (fromLIppK g (clCub x))

uncurryCl :: (BdCub b -> OCub b -> a) -> (ClCub b -> a)
uncurryCl f x = f (clBoundary x) (clInterior x)


clCubPick :: Address -> ClCub a -> Either Int (ClCub a)
-- clCubPick (Address sfc []) x =
--    Right $ clCubPickSF sfc x


clCubPick (Address sfc addr@(_ : _)) x@(ClCub fli)
   | isFullSF sfc =
      case (reverse addr , appLI sfc fli)  of
        (AOnCylinder sf : xs , Hcomp _ _ cy y) -> undefined
           -- case appLI sf (cylCub si) of
           --   Just y -> undefined --first (1 +) (oCubPick (reverse xs) y)
           --   Nothing -> Left 0
        (AOnBottom sf : xs , Hcomp _ _ _ y) ->
            first (1 +) (clCubPick (Address sf (reverse xs)) y)
        (_ : _ , _) ->
            Left 0
        ([] , _) -> Right x
   | otherwise =
       first (1 +)
       $ clCubPick (Address (fullSF (subFaceDimEmb sfc)) addr) (clCubPickSF sfc x)

clCubPickO :: Address -> ClCub a -> Either Int (OCub a)
clCubPickO (Address sfc addr) (ClCub fli) =
    first (1 +) $ oCubPick addr (appLI sfc fli)



oCubPick :: [AddressPart] -> OCub a -> Either Int (OCub a)
oCubPick [] x = Right x
oCubPick (_ : _) x@Cub {} = Left 0
oCubPick addr (Hcomp _ _ si a) =
   case reverse addr of
     AOnCylinder sf : xs ->
        case appLI sf (cylCub si) of
          Just y -> first (1 +) (oCubPick (reverse xs) y)
          Nothing -> Left 0
     AOnBottom sf : xs ->
         first (1 +) (clCubPickO (Address sf (reverse xs)) a)
     _ -> error "imposible, this case should be handled in ther firt clause"


cubMapShallow ::  forall a b. (Monad a) =>
                  (b -> a b)
                 -> OCub b
                 -> a (OCub b)
cubMapShallow f (Cub n b mbc) =
  do b2 <- f b
     return (Cub n b2 mbc)
cubMapShallow f (Hcomp b mbNm cyl btm) =
  do b2 <- f b
     return (Hcomp b2 mbNm cyl btm)

cubMapTrav ::  forall a b bb. (Monad a) =>
             (Int -> Address -> b -> Maybe Name -> CylCub b -> ClCub b -> a bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> a bb)
                 -> ClCub b
                 -> a (ClCub bb)
cubMapTrav g f (ClCub xx) = ClCub <$>
   traverseMapLI (\sf -> cm (Address sf [])) xx

  where
     cm :: (Monad a) => Address -> OCub b -> a (OCub bb)
     cm addr@(Address _ tla) (Cub n b mbc) =
          do b2 <- f n addr b mbc
             return $ Cub n b2 mbc

     cm addr@(Address sf0 tla) ocub@(Hcomp b mbNm cyl btm) =
          do b2 <- g (getDim ocub) addr b mbNm cyl btm
             cyl2 <- CylCub <$> traverseMapLI
                       (\sf -> maybe (return Nothing) (fmap Just . cm (Address sf0 (AOnCylinder sf : tla))))
                   (cylCub cyl)
             btm2 <- ClCub <$> traverseMapLI (\sf -> cm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
             return (Hcomp b2 mbNm cyl2 btm2)

cubMapTrav' ::  forall a b bb. (Monad a) =>
             (Int -> Address -> b -> Maybe Name -> CylCub bb -> ClCub bb -> a bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> a bb)
                 -> ClCub b
                 -> a (ClCub bb)
cubMapTrav' g f (ClCub xx) = ClCub <$>
   traverseMapLI (\sf -> cm (Address sf [])) xx

  where
     cm :: (Monad a) => Address -> OCub b -> a (OCub bb)
     cm addr@(Address _ tla) (Cub n b mbc) =
          do b2 <- f n addr b mbc
             return $ Cub n b2 mbc

     cm addr@(Address sf0 tla) ocub@(Hcomp b mbNm cyl btm) =
          do cyl2 <- CylCub <$> traverseMapLI
                       (\sf -> maybe (return Nothing) (fmap Just . cm (Address sf0 (AOnCylinder sf : tla))))
                   (cylCub cyl)
             btm2 <- ClCub <$> traverseMapLI (\sf -> cm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
             b2 <- g (getDim ocub) addr b mbNm cyl2 btm2
             return (Hcomp b2 mbNm cyl2 btm2)



cubMapTravWithB ::  forall a b bb. (Monad a) =>
             (Int -> Address -> BdCub b -> b -> Maybe Name -> CylCub b -> ClCub b -> a bb)
          -> (Int -> Address -> BdCub b -> b -> Maybe CellExpr -> a bb)
                 -> ClCub b
                 -> a (ClCub bb)
cubMapTravWithB g f xxx@(ClCub xx) = ClCub <$>
   traverseMapLI (\sf _ -> uncurryCl (cm (Address sf [])) (clCubPickSF sf xxx)) xx

  where
     cm :: (Monad a) => Address -> BdCub b -> OCub b -> a (OCub bb)
     cm addr@(Address _ tla) bd (Cub n b mbc) = 
          do b2 <- f n addr bd b mbc
             return $ Cub n b2 mbc

     cm addr@(Address sf0 tla) bd ocub@(Hcomp b mbNm cyl btm) = 
          do b2 <- g (getDim ocub) addr bd b mbNm cyl btm
             let cylBd :: SubFace -> BdCub b
                 cylBd sf = BdCub $ elimSubFaceSideLI
                          (clCub (bdCubPickSF sf bd))
                          (clCub (clCubPickSF sf btm))
                          (FromLI (subFaceDimEmb sf) ( \sf' -> fromJust $ appLI (injSubFace sf' sf) (cylCub cyl) ))

             cyl2 <- CylCub <$> traverseMapLI
                       (\sf -> maybe (return Nothing)
                              (fmap Just . cm (Address sf0 (AOnCylinder sf : tla)) (cylBd sf)))
                       (cylCub cyl)
             btm2 <- ClCub <$> traverseMapLI (\sf _ ->
                                                  uncurryCl (cm (Address sf0 (AOnBottom sf : tla)))
                                                  (clCubPickSF sf btm)  ) (clCub btm)
               -- traverseMapLI (\sf -> cm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
             return (Hcomp b2 mbNm cyl2 btm2)



cubMapWithB ::
             (Int -> Address -> BdCub b -> b -> Maybe Name -> CylCub b -> ClCub b -> bb)
          -> (Int -> Address -> BdCub b -> b -> Maybe CellExpr -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMapWithB f g = runIdentity . cubMapTravWithB (dot7 Identity f) (dot5 Identity  g)
-- -- instead of ClCub Boundary shoudl be used here...
-- cubMapTravWithBd ::  forall a b. (Monad a) =>
--              (Int -> Address -> ClCub b -> b -> a b)
--                  -> ClCub b
--                  -> a (ClCub b)

-- cubMapTravWithBd f clc = ClCub <$>
--    traverseMapLI (\sf -> cm (subFaceDimEmb sf) (Address sf []))
--        (FromLI (getDim clc) (`clCubPickSF` clc))

--   where
--      cm :: (Monad a) => Int -> Address -> ClCub b -> a (OCub b)
--      cm n addr clc = do
--        case clInterior clc of
--          Cub {} -> undefined
--          Hcomp {} -> undefined
--        clc2 <- undefined
--        undefined

--      -- cm addr@(Address sf0 tla) ocub@(Hcomp b mbNm cyl btm) = undefined


cubMap :: (Int -> Address -> b -> Maybe Name -> CylCub b -> ClCub b -> bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMap f g = runIdentity . cubMapTrav (dot6 Identity f) (dot4 Identity  g)

cubMap' :: (Int -> Address -> b -> Maybe Name -> CylCub bb -> ClCub bb -> bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMap' f g = runIdentity . cubMapTrav' (dot6 Identity f) (dot4 Identity  g)

-- cubFill :: Monoid b => (Int -> Address -> FromLI SubFace b -> SubFace -> b)
--                  -> ClCub b
--                  -> ClCub b
-- cubFill f = cubMap ff (\_ _ b _ -> b) 

--   where
--     ff :: Int -> Address -> b -> Maybe Name -> CylCub b -> ClCub b -> b
--     ff n addr b mbn si ce = undefined

cubMapWAddr :: (Address -> b -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMapWAddr f = cubMap (\_ addr b _ _ _ -> f addr b) (\_ addr b _ -> f addr b)




cubMapMayReplace  :: forall a b . (Monad a) =>  (Int -> Address -> OCub b -> a (Maybe (OCub b)))
                 -> ClCub b
                 -> a (ClCub b)
cubMapMayReplace f (ClCub xx) = ClCub <$>
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
                do cyl2 <- CylCub <$> traverseMapLI
                       (\sf -> maybe (pure Nothing) (fmap Just . cmm (Address sf0 (AOnCylinder sf : tla))))
                        (cylCub cyl)
                   btm2 <- ClCub <$> traverseMapLI (\sf -> cmm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
                   return (Hcomp b mbNm cyl2 btm2)

             Cub {} -> return x


-- -- variant of cubMap constant on node data
cubMapOld = cubMap (const $ const $ \b -> const $ const $ const b)



-- cubMapFill :: forall a b . (Monad a) => (Int -> Address -> (CylCub b , ClCub b) -> a (CylCub b)  )
--                  -> ClCub b
--                  -> a (ClCub b)
-- cubMapFill f (ClCub xx) = ClCub <$>
--    traverseMapLI (\sf -> cmf (Address sf [])) xx

--   where
--     cmf :: (Monad a) =>  Address
--                  -> OCub b
--                  -> a (OCub b)
--     cmf addr@(Address sf0 tla) ocub@Cub {} = return ocub
--     cmf addr@(Address sf0 tla) ocub@(Hcomp b mbNm cyl btm) = do
--         cyl2 <- CylCub <$> traverseMapLI
--                        (\sf -> maybe (pure Nothing) (fmap Just . cmf (Address sf0 (AOnCylinder sf : tla))))
--                         (cylCub cyl)
--         btm2 <- ClCub <$> traverseMapLI (\sf -> cmf (Address sf0 (AOnBottom sf : tla))) (clCub btm)
--         cylFilled <- f (getDim ocub) addr (cyl2 , btm2)

--         return $ Hcomp b mbNm cylFilled btm2

instance Foldable OCub where
  foldMap f (Cub _ b _) = f b
  foldMap f (Hcomp b _ cyl btm) =  f b <> foldMap f cyl <> foldMap f btm

instance Foldable ClCub where
  foldMap f (ClCub w) = foldMap (foldMap f) w

instance Foldable CylCub where
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




toClCub :: (Env , Context) -> Expr -> ClCub ()

toClCub  (env , ct@(Context _ dims)) expr =
  -- trace ("\n\n" ++  show dims ++ "\n" ++ toString (env , ct) expr ++ "\n")

         ClCub $ fromLIppK
                  (\sfc ->
                        -- trace ( (show (getDim (env , addSFConstraintToContext sfc ct)))  ++ "\n---\n" ++ show ee ++ "\n---\n"
                        --                  ++  show (toOCub ((env , addSFConstraintToContext sfc ct) , e))
                        --                  ++ "\n---\n")
                        toOCub (env , addSFConstraintToContext sfc ct)
                        -- if (isFullSF sfc)
                        -- then toOCub ((env , addSFConstraintToContext sfc ct) , e)
                        -- else (Cub 0 () Nothing)
                          -- if (subFaceCodim sfc == 1)
                        --      then (toOCub ((env , addSFConstraintToContext sfc ct) , e))
                        --      else (Cub 0 () Nothing)
                        )
                     (exprSubFaces ct expr)
--    ClCub (FromLI (getDim ct) (\sf -> Cub (subFaceDimEmb sf) () Nothing))

-- toClCub  (ee@(env , ct) , tm@(Var vI tl)) =
--   ClCub (FromLI (getDim ct) (\sf -> Cub (subFaceDimEmb sf) () Nothing))

-- toClCub  ee@((env , ct) , (HComp nam pa e)) = undefined



toOCub :: (Env , Context) -> Expr -> OCub ()


toOCub (env , ct) (Hole hI) = Cub (getDim ct) () Nothing

toOCub ee@(env , ct) tm@(Var vI tl) = Cub (getDim ct) () $
    Just $ mkCellExpr ee vI (fmap snd tl)



toOCub ee@(env , ct) expr@(HComp nam pa e) =
       let ct2 = addDimToContext ct nam
           dim = getDim ee
           cyl = CylCub $
                 fromMapFLI dim $
                 Map.mapKeys (sf2ToSubFace ct) $
                 Map.mapWithKey (\sf2 ->
                                    -- trace ( "\n---\n" ++ show ee ++ "\n---\n" ++ show sf2 ++ "\n---\n" ++ "\n---\n"
                                    --      -- ++  show ((toOCub $ ((env , addSF2ConstraintToContext sf2 ct2),) e))
                                    --      ++ "\n---\n")
                                    toOCub (env , addSF2ConstraintToContext sf2 ct2)
                                ) $
                 partialWithSF ct nam
                 pa

           btm = --trace ("\n\nzzz\n" ++ toString (env , ct) expr ++ "\n")
                  toClCub (env , ct) e
       in Hcomp () nam cyl btm


fromCylCub :: (Env , Context) -> Maybe Name -> CylCub () -> Partial
fromCylCub (ee , ctx) nm =
  let  ctx' = addDimToContext ctx nm
  in
           Map.mapWithKey (\sf2 -> fromOCub (ee , addSF2ConstraintToContext sf2 ctx' ) )
         . makeAntiHKeys
         . Map.mapMaybe id
         . Map.mapKeys (sfToSubFace2 ctx)
         . toMapFLI
         . cylCub




  --makeAntiHKeys



fromOCub :: (Env , Context) -> OCub () -> Expr

fromOCub ee (Cub _ _ mbCe) = maybe (Hole 0) (fromCellExprSafe ee) mbCe

fromOCub ee (Hcomp b nm cyl btm) =
  HComp nm
  (fromCylCub ee nm cyl)
  (fromClCub ee btm)



fromClCub :: (Env , Context) -> ClCub () -> Expr
fromClCub ee = fromOCub ee . getCenter  . clCub

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






contextAt :: Context -> Address -> ClCub b -> Context
contextAt = undefined
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

data Direction = DParent | DChild | DNext | DPrev

data ImposibleMove = ImposibleMove


negateDimIndexes :: Set.Set Int -> ClCub () -> ClCub ()
negateDimIndexes = undefined
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


rotateDimIndexes :: (Int , Int) -> ClCub () -> ClCub ()
rotateDimIndexes = undefined
-- rotateDimIndexes ( i , j ) =    
--    permuteDimIndexes (Set.fromList [i , j])
--  . negateDimIndexes (Set.fromList [i])

permuteDimIndexes :: Set.Set Int -> ClCub () -> ClCub ()
permuteDimIndexes = undefined
-- permuteDimIndexes dims cub
--   | Set.size dims <= 1  = cub
--   | otherwise = remapDimIndexes
--        (\k ->
--          if Set.member k dims
--          then rotateFrom k (Set.toList dims)
--          else k) cub


-- -- f must be bijection !!! 
remapDimIndexes :: (Int -> Int) -> ClCub () -> ClCub ()
remapDimIndexes = undefined
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

cubNav :: ClCub a -> Address -> Direction -> Either ImposibleMove Address

cubNav c addr dir = undefined
  -- case (cubPick addr c , addr , dir) of
  --   (Nothing , _ , _) -> error "bad address!"

  --   (Just _ , (x : xs) , DParent ) -> Right xs
  --   (Just _ , [] , DParent ) -> Left ImposibleMove
  --               -- top level cell do not have parent!

  --   (Just (Cub _ _) , _ , DChild ) -> Left ImposibleMove
  --               -- leaf do not have childern!
  --   (Just (Hcomp _ _ _ a) , _ , DChild ) -> Right (fullSF (getDim a) : addr)
  --               -- leaf do not have childern!                                     


  --   (Just _ , [] , DNext ) -> Left ImposibleMove
  --   (Just _ , [] , DPrev ) -> Left ImposibleMove
  --               -- top level cell do not have a parent, so it is not posible to cycle thru its children

  --   (Just _ , x : xs , nv ) ->
  --      case cubPick xs c of         
  --        Just (Hcomp _ _ pa a) -> 
  --            let
  --                sfcs = fullSF (getDim a) : (Map.keys pa)
  --                x2 =
  --                  case nv of
  --                    DNext -> rotateFrom x sfcs 
  --                    DPrev -> rotateFrom x (reverse sfcs)
  --                    _ -> error "imposible"
  --            in Right (x2 : xs)
  --        _ -> error "bad address, parent cell is leaf!!"


vaccantSubFaces :: ClCub a -> Address -> Set.Set SubFace
vaccantSubFaces cub addr =
  case clCubPickO addr cub of
    Right (Hcomp _ _ pa a) -> missingSubFaces (getDim a)
                              $ Map.keysSet (Map.filter isJust (toMapFLI (cylCub pa)))
    _ -> Set.empty
