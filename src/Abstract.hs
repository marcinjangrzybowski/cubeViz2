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


import qualified Data.Foldable as Foldable

import Data.Functor.Identity

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

import Debug.Trace
import Data.Either (fromRight)

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


isHole :: OCub b -> Bool
isHole (Cub _ _ Nothing) = True
isHole _ = False 
-- instance Bifunctor Cub where
--   first f (Cub fc a) = (Cub (fmap (first f) fc) a) 
--   first f (Hcomp b nm si a) = (Hcomp (f b) nm (fmap (first f) si) (first f a))

--   second = fmap



data AddressPart = AOnCylinder SubFace | AOnBottom SubFace
  deriving (Show , Eq , Ord)
data Address = Address SubFace [AddressPart]
  deriving (Show , Eq, Ord)

newtype CAddress = CAddress {cAddress :: (Set.Set Address)}
  deriving (Show , Eq , Ord)


data CStatus = CSConstrained
  deriving (Show , Eq , Ord)

-- instance Eq (CAddress b) where
--   (CAddress a) == (CAddress _ b) = a == b 

-- instance Ord (CAddress b) where
--   (CAddress _ a) <= (CAddress _ b) = a <= b 


-- isTornAddress :: (ClCub b) -> Address -> Bool
-- isTornAddress cub addr = undefined 

-- addressClass :: (ClCub b) -> Address -> (Set.Set Address)
-- addressClass cub addr = undefined 




addresedDimPart :: AddressPart -> Int
addresedDimPart (AOnCylinder sf) = subFaceDimEmb sf + 1
addresedDimPart (AOnBottom sf) = subFaceDimEmb sf


addresedDim :: Address -> Int
addresedDim (Address sf []) = subFaceDimEmb sf
addresedDim (Address _ (x : _)) = addresedDimPart x

cAddresedDim :: CAddress -> Int
cAddresedDim = addresedDim . head . Set.toList . cAddress


onCyl :: Address -> SubFace -> Address
onCyl addr@(Address x y) sf
  | addresedDim addr == getDim sf = Address x (AOnCylinder sf : y)
  | isFullSF sf = error "fullSF cannot be put in Cyliner part of address"
  | otherwise = error "Address aprt do not match rest of address"

onBottom :: Address -> SubFace -> Address
onBottom addr@(Address x y) sf
  | addresedDim addr == getDim sf = Address x (AOnBottom sf : y)
  | otherwise = error "Address aprt do not match rest of address"

injAddress :: SubFace -> Address -> Address
injAddress sf (Address sf' addr') = Address (injSubFace sf' sf ) addr'

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

instance OfDim AddressPart where
  getDim (AOnCylinder sf) = getDim sf
  getDim (AOnBottom sf) = getDim sf


-- todo TEST IT!!
-- first argument is possible parent
-- result is adress to apply in chain with first address to obtain same result as applying only second address 


mbSubAddressTest =
   [mbSubAddress
    (Address (SubFace 2 (Map.fromList ([(0,True),(1,False)])) ) [])
    (Address (SubFace 2 (Map.fromList ([(0,b1),(1,b2)])) ) [])
   | b1 <- [True , False] , b2 <- [True , False]  ]

mbSubAddress :: Address -> Address -> Maybe Address
mbSubAddress (Address sf addr) (Address sf' addr') 
  | not (sf' `isSubFaceOf`  sf) = Nothing
  
  | null addr =
         Just (Address (jniSubFace sf' sf) addr')
         
  | sf == sf' = sameSFHelp (reverse addr) (reverse addr')
  
  | not (null addr) && not (isFullSF sf) = 
                  mbSubAddress
                      (Address (fullSF (subFaceDimEmb sf)) addr)
                      (Address (jniSubFace sf' sf) addr')

  | not (null addr) && isFullSF sf = bdHelp (reverse addr) sf' (reverse addr')


   where
     sameSFHelp :: [AddressPart] -> [AddressPart] -> Maybe Address
     sameSFHelp [] _ = error "cannot be called for empty first arg"
     sameSFHelp (_ : _) [] = Nothing 
     sameSFHelp (AOnCylinder sf : addr) (AOnCylinder sf' : addr') =
         if (isSubFaceOf sf' sf) then mbSubAddress
                 (Address (injCyl sf) (reverse addr))
                 (Address (injCyl sf') (reverse addr')) else Nothing

     sameSFHelp (AOnBottom sf : addr) (AOnCylinder sf' : addr') = Nothing

     sameSFHelp (AOnCylinder sf : addr) (AOnBottom sf' : addr') =
       case jniSubFaceMb sf' sf of
          Nothing -> Nothing
          Just sf'' ->
               let z = mbSubAddress
                       (Address (fullSF (subFaceDimEmb sf + 1)) (reverse addr))
                       (Address (injCylEnd False sf'') (reverse addr'))
               in z

     sameSFHelp (AOnBottom sf : addr) (AOnBottom sf' : addr') =
        mbSubAddress
          (Address sf (reverse addr))
          (Address sf' (reverse addr'))



     bdHelp [] _ _ = error "cannot be called for empty first arg"

     bdHelp (AOnCylinder sf : addr) sf' addr' =
       case jniSubFaceMb sf' sf of
          Nothing -> Nothing
          Just sf'' ->
               let z = mbSubAddress
                       (Address (fullSF (subFaceDimEmb sf + 1)) (reverse addr))
                       (Address (injCylEnd True sf'') (reverse addr'))
               in z

     bdHelp (AOnBottom sf : addr) _ _ = Nothing




--mbSubAddress

-- first arg is possible parent
mbSubFaceAddr :: Address -> Address -> Maybe SubFace
mbSubFaceAddr a1 a2 =
  case mbSubAddress a1 a2 of
     Just (Address sf []) -> Just sf
     _ -> Nothing

-- toUAddress :: (ClCub b) -> Address -> Maybe (UAddress b)  
-- toUAddress cub addr | not (isValidAddressFor cub addr) = Nothing 
-- toUAddress cub a@(Address sf addr) = undefined

toFillParentOPart :: OCub b -> AddressPart -> Set.Set (SubFace , AddressPart)

toFillParentOPart (Hcomp _ _ cyl bd) (AOnCylinder sf) =
  Set.map (\sf' -> (sf' , AOnCylinder (jniSubFace sf sf')))
  $ superFacesOutside (occupiedCylCells cyl) sf
toFillParentOPart (Hcomp _ _ cyl bd) (AOnBottom sf) =
  Set.map (\sf' -> (sf' , AOnBottom (jniSubFace sf sf'))) 
  $ superFacesOutside (occupiedCylCells cyl) sf
  
toFillParentO' :: OCub b -> [AddressPart] -> Set.Set Address
toFillParentO' ocub [] = Set.empty
toFillParentO' ocub@Cub {} (x : xs) = error "bad addr"
toFillParentO' ocub@Hcomp {} (x : xs) =
  let z = toFillParentOPart (fromRight (error "badAddress") $ oCubPick xs ocub) x
  in  Set.map
      (\(sf , aPart) ->
         (\(Address sf' ys) -> Address sf' (aPart : ys)  ) (addressSubFace (Address (fullSF (getDim ocub)) xs) sf) )
      z

toFillParentO :: OCub b -> [AddressPart] -> Set.Set Address
toFillParentO ocub tl =
  let z = toFillParentO' ocub tl
  in case ((Set.null z) , tl) of       
       (True , x : xs) ->
           Set.map (\(Address sf y) -> Address sf (x : y)) 
         $ toFillParentO ocub xs 
       _ -> z

toFillParent :: ClCub b -> Address -> Set.Set Address
toFillParent cub (Address sf addr) =
       Set.map (injAddress sf)
     $ toFillParentO (clInterior (clCubPickSF sf cub)) addr

-- toFillParent :: ClCub b -> Address -> Set.Set Address
-- toFillParent cub a@(Address sf addr) =
--    let tfp' = toFillParent' cub a
--    in case Set.null (toFillParent' cub a) of
--          False -> tfp'
--          True -> undefined

-- TODO: Test it!
getAllAddrClasses :: ClCub b -> Set.Set CAddress
getAllAddrClasses cub =
  Set.map CAddress $ disjointFam
  $ disjointSetFamFold
  $ cubMapWAddr (\addr _ -> Set.insert addr (toFillParent cub addr)) cub

addressClass :: ClCub b -> Address -> CAddress
addressClass cub addr =   
  fromJust $ find (Set.member addr . cAddress) $ getAllAddrClasses cub


-- mkCAddress :: ClCub b -> Address -> CAddress
-- mkCAddress cub addr = 
--   fromJust $ find (Set.member addr . cAddress) $ getAllAddrClasses cub

-- TODO: invesitagate performance here
compAddressClass :: ClCub b -> Address -> Address -> Bool
compAddressClass cub addrA addrB =
  any ((Set.isSubsetOf $ Set.fromList [addrA , addrB]) . cAddress) (getAllAddrClasses cub)
  
isInternalAddress :: Address -> Bool
isInternalAddress (Address sf tl) = isFullSF sf && not (null tl)
-- isInternalAddress (Address sf _) | not $ isFullSF sf = False 
-- isInternalAddress (Address _ []) = False
-- isInternalAddress _ = True

-- this ingore fillings (can produce wrong addresses when there are missing faces in compositions)
addressSuperFaces :: Address -> [Address]
addressSuperFaces (Address sf []) = [ Address ssf [] | ssf <- superSubFaces sf ]
addressSuperFaces (Address sf ((AOnBottom sfA) : xs)) | isFullSF sfA  = []
addressSuperFaces (Address sf ((AOnBottom sfA) : xs)) =
     Address sf (AOnCylinder sfA : xs ) :
   [ Address sf (AOnBottom ssf : xs ) | ssf <- superSubFaces sfA ]

addressSuperFaces (Address sf ((AOnCylinder sfA) : xs)) =
   [ Address sf (AOnCylinder ssf : xs ) | ssf <- filter (not . isFullSF) $ superSubFaces sfA ]

-- --this is more usefull, it only gives addreses of Leafs
-- addressSuperCells :: ClCub a -> Address -> [Address]
-- addressSuperCells cub (Address sf []) | isFullSF sf = []
-- addressSuperCells cub (Address sf []) | subFaceCodim sf == 1
--      = undefined

-- addressSuperCells cub (Address sf []) =
--   [ let asc = addressSuperCells (clCubPickSF ssf cub) (Address (fullSF (subFaceDimEmb ssf)) [])  

--     in case asc of
--           x : _ -> undefined
--           _ -> undefined 

--   | ssf <- superSubFaces sf ]

-- addressSuperCells cub (Address sf tl) = undefined



-- unsafe, may throw error
addressSubFace :: Address -> SubFace -> Address
addressSubFace addr sf | addresedDim addr /= getDim sf = error "sub face dim do not match addresed dim"
addressSubFace addr sf | isFullSF sf = addr
addressSubFace (Address sf' []) sf = Address (injSubFace sf sf') []

addressSubFace (Address sf' ((AOnBottom sfA) : xs)) sf =
                 Address sf' ((AOnBottom (injSubFace sf sfA)) : xs)

addressSubFace (Address sf' ((AOnCylinder sfA) : xs)) sf =
   case jniCyl sf sfA of
     JCEnd True sff -> addressSubFace (Address sf' xs) sff
     JCEnd False sff -> Address sf' ((AOnBottom sff) : xs)

     JCCyl sff -> Address sf' ((AOnCylinder sff) : xs)


cAddrWithSubFaces :: ClCub a -> CAddress -> Set.Set CAddress
cAddrWithSubFaces cub x =
  Set.fromList
  [ addressClass cub (addressSubFace (head $ Set.toList $ cAddress x ) sf)
  | sf <- (genAllLI (cAddresedDim x)) ]



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
        (AOnCylinder sf : xs , Hcomp _ _ cy y) -> 
           case appLI sf (cylCub cy) of
             Just y -> undefined --todo --first (1 +) (oCubPick (reverse xs) y)
             Nothing -> Left 0
        (AOnBottom sf : xs , Hcomp _ _ _ y) ->
            first (1 +) (clCubPick (Address sf (reverse xs)) y)
        (_ : _ , _) ->
            Left 0
        ([] , _) -> Right x
   | otherwise =
       first (1 +)
       $ clCubPick (Address (fullSF (subFaceDimEmb sfc)) addr) (clCubPickSF sfc x)

clCubPickData :: Address -> ClCub a -> Maybe a
clCubPickData a x =
  case clCubPickO a x of
    Right (Cub _ a _) -> Just a
    Right (Hcomp a _ _ _) -> Just a
    _ -> Nothing

oCubPickTopLevelData :: OCub a -> a
oCubPickTopLevelData x =
  case x of
    (Cub _ a _) -> a
    (Hcomp a _ _ _) -> a

clCubPickTopLevelData :: ClCub a -> a
clCubPickTopLevelData =  oCubPickTopLevelData . clInterior 
  
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


-- truncates Address so it will be valid for given complex
-- may throw error for bad dim
fixAddress :: ClCub a -> Address -> Address
fixAddress y addr | getDim y /= getDim addr = error "dimensions of address and xomplex do not match!"
fixAddress y addr@(Address sf tl) =
  case oCubPick tl $ clInterior $ clCubPickSF sf y of
    Right _ -> addr
    Left i -> Address sf $ reverse $ take i (reverse tl)

isValidAddressFor :: ClCub a -> Address -> Bool
isValidAddressFor y addr | getDim y /= getDim addr = error "dimensions of address and xomplex do not match!"
isValidAddressFor y addr@(Address sf tl) =
  case oCubPick tl $ clInterior $ clCubPickSF sf y of
    Right _ -> True
    Left _ -> False

addressClassSuperCellsClass :: ClCub a -> CAddress -> Set.Set CAddress
addressClassSuperCellsClass cl x = 
    foldl Set.union Set.empty
  $ Set.map (Set.fromList . (fmap $ addressClass cl) . (addressSuperCells cl)) $ cAddress x

addressClassSuperCellsClassLeafs :: ClCub a -> CAddress -> Set.Set CAddress
addressClassSuperCellsClassLeafs cl x =  
    foldl Set.union Set.empty
  $ Set.map (Set.fromList . (fmap $ addressClass cl) . (onlyLeafs . addressSuperCells cl)) $ cAddress x


directlyConstrainingSuperCells :: ClCub a -> CAddress -> Set.Set CAddress
directlyConstrainingSuperCells cub x = 
    Set.filter (\a' -> let a = head (Set.toList $ cAddress a') in not . isHole $  fromRight (error "imposible") (clCubPickO a cub) )
  $ addressClassSuperCellsClassLeafs cub x 


isFreeAddressClass :: ClCub a -> CAddress  -> Bool
isFreeAddressClass cub x = Set.null $ directlyConstrainingSuperCells cub x  


isFreeAddressClassRel :: ClCub a -> Set.Set CAddress -> CAddress  -> Bool
isFreeAddressClassRel cub y x = Set.null $ Set.difference y $ directlyConstrainingSuperCells cub x  



onlyLeafs :: [Address] -> [Address]
onlyLeafs xs =
  let f x = all (\y -> x == y || (isNothing (mbSubAddress x y) )  ) xs
  in filter f xs
  

-- addresses for which given addres is FACE
addressSuperCells :: ClCub a -> Address -> [Address]
addressSuperCells cl addr =
   let cl' = fmap (const []) cl

       z n addr' _ =
         [addr' | addr `elem` [ addressSubFace addr' (toSubFace fc) | fc <- (genAllLI n)]]

   in Foldable.fold $ cubMapOldBoth z cl'


-- TODO : create eficient, less naive , implementation




-- not really usefull
traceConstraintsSingleAll :: forall a. ClCub a -> CAddress -> ClCub (Maybe SubFace , a)
traceConstraintsSingleAll cl caddrs =
      cubMapOldBoth f cl

  where

    addrS = cAddress caddrs
    
    f n addr a =
      let sfs = catMaybes $ Set.toList (Set.map (mbSubFaceAddr addr) addrS)

          sf = case sfs of
                 [] -> Nothing
                 [x] -> Just x
                 (_ : _) -> error "imposible ??"
        
      in (sf , a)

traceConstraintsSingle :: forall a. ClCub a -> CAddress -> ClCub (Maybe SubFace , a)
traceConstraintsSingle cl caddrs =
      cubMap' nf cf  cl

  where

    addrS = cAddress caddrs

    cf n addr a _ =
      let sfs = catMaybes $ Set.toList (Set.map (mbSubFaceAddr addr) addrS)

          sf = case sfs of
                 [] -> Nothing
                 [x] -> Just x
                 (_ : _) -> error "imposible ??"
        
      in (sf , a)

    
    nf n addr a _ cy btm = 
      let insQ = any (isJust . fst) cy || any (isJust . fst) btm        
      in if insQ
         then (Nothing , a)
         else (cf n addr a undefined)


traceConstraints :: forall a. ClCub a -> Set.Set CAddress -> ClCub (Maybe Int , a)
traceConstraints cl caddrs =
      cubMap' nf cf cl

  where

    addrS = Set.unions $ Set.map cAddress caddrs

    cf n addr a _ =
      let sfs = catMaybes $ Set.toList (Set.map (mbSubFaceAddr addr) addrS)

          sf = case sfs of
                 [] -> Nothing
                 (_ : _) -> Just (maximum $ fmap (subFaceDimEmb) sfs)
                 -- (_ : _) -> error "imposible ??"
        
      in (sf , a)

    
    nf n addr a _ cy btm = 
      let 
          insQ :: Int
          insQ = Prelude.max (maximum (fmap ((maybe (-1) id ) . fst) cy))
                             (maximum (fmap ((maybe (-1) id ) . fst) btm) )

          ownQ = fst (cf n addr a undefined)
      in case ownQ of
           Nothing -> (Nothing , a)
           Just ownK -> 
               if (ownK > insQ)
               then (ownQ , a)
               else (Nothing , a)



  --  j ( 0 , caddrs , fmap ( (,) Nothing ) cl)

  -- where

  --   h :: (Int , Set.Set (CAddress) , ClCub (Maybe Int , a)) -> (Int , Set.Set (CAddress) , ClCub (Maybe Int , a))
  --   h (k , sc , cub) =
  --     let cub0 = cubMapOldBoth
  --                 (\n addr x -> 
  --                     let (x0 , x1) =
  --                           (if (any (\ca -> Set.member addr (cAddress ca) ) sc )
  --                           then (Just k , addressClassSuperCellsClassLeafs cub (addressClass cub addr))
  --                            else (Nothing , Set.empty))
  --                     in (x0 , x1 , x)
  --                 )
  --                 cub
                        
  --         sc0 = foldl' (\s (x , sccs , _) -> Set.union s sccs) Set.empty cub0

  --         f (Just x) _ = Just x
  --         f _ (Just x) = Just x
  --         f _ _ = Nothing
          
  --     in --trace (show k ++ show (Set.size sc0))
  --        (k + 1 , sc0 , fmap (\(x , _ , (y , a)) -> (f x y , a) ) cub0)


    
  --   j :: (Int , Set.Set (CAddress) , ClCub (Maybe Int , a)) -> ClCub (Maybe Int , a) 
  --   j (k , s , x) | null s = x
  --                 | otherwise = j (h (k , s , x))      
    

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
                          (clCub (clCubPickSF sf btm))
                          (clCub (bdCubPickSF sf bd))
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

clClearAllCells :: ClCub [b] -> ClCub [b]
clClearAllCells = cubMapWAddr (\_ _ -> [])


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

cubMapOldBoth f = cubMap (\n addr b _ _ _ -> f n addr b) (\n addr b _ -> f n addr b)




travOldBoth :: forall a b b2 . (Monad a) =>
                 (Int -> Address -> b -> a b2)
                 -> ClCub b -> a (ClCub b2)
travOldBoth f = cubMapTrav' (\n addr b _ _ _ -> f n addr b) (\n addr b _ -> f n addr b)


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


cubNav c a@(Address sf addr) dir =
  case (clCubPickO a c , addr , dir) of
    (Left _ , _ , _) -> error "bad address!"

    (Right _ , x@(AOnBottom sf') : xs , DParent ) ->
       if isFullSF sf'
       then Right (Address sf xs)
       else Right (Address sf (AOnBottom (fullSF (getDim sf')) : xs) )

    (Right _ , x : xs , DParent ) -> Right (Address sf xs)

    (Right _ , [] , DParent ) ->
       if isFullSF sf
       then Left ImposibleMove
       else Right (Address (fullSF (getDim sf) ) [])
                -- top level cell do not have parent!

    (Right (Cub {}) , _ , DChild ) -> Left ImposibleMove
                -- leaf do not have childern!
    (Right (Hcomp _ _ _ btm) , _ , DChild ) -> Right (Address sf (AOnBottom (fullSF (getDim btm)) : addr))
                -- leaf do not have childern!                                     


    (Right _ , [] , DNext ) -> Left ImposibleMove
    (Right _ , [] , DPrev ) -> Left ImposibleMove
                -- top level cell do not have a parent, so it is not posible to cycle thru its children

    (Right _ , x : xs , nv ) ->
       case (clCubPickO (Address sf xs) c , x) of
         (Right (Hcomp _ _ pa a) , _) ->
             let
                 sfcs = AOnBottom (fullSF (getDim a))
                          : fmap AOnCylinder (Set.toList
                                                      (let s = occupiedCylCells pa
                                                       in (Set.filter (not . isCoveredIn s) s )))
                 x2 =
                   case nv  of
                     DNext -> rotateFrom x sfcs
                     DPrev -> rotateFrom x (reverse sfcs)
                     _ -> error "imposible"
             in if x `elem` sfcs
                then Right (Address sf (x2 : xs))
                else Left ImposibleMove
         _ -> error "bad address, parent cell is leaf!!"


-- cubNavFSF :: ClCub a -> Address -> Address -> Direction -> Either ImposibleMove Address
-- cubNavFSF c a a' d =
--   case d of
--     DNext -> cubNavF c a a' True
--     DPrev -> cubNavF c a a' False
--     DParent -> cubNavSF c a a' True
--     DChild -> cubNavSF c a a' False

-- cubNavF :: ClCub a -> Address -> Address -> Bool -> Either ImposibleMove Address      
-- cubNavF c a a' b =
--   case SF

-- cubNavSF :: ClCub a -> Address -> Address -> Bool -> Either ImposibleMove Address      
-- cubNavSF c a a' b = undefined


vaccantSubFaces :: ClCub a -> Address -> Set.Set SubFace
vaccantSubFaces cub addr =
  case clCubPickO addr cub of
    Right (Hcomp _ _ pa a) -> missingSubFaces (getDim a)
                              $ Map.keysSet (Map.filter isJust (toMapFLI (cylCub pa)))
    _ -> Set.empty

occupiedCylCells :: CylCub a -> Set.Set SubFace
occupiedCylCells cc = keysSetMapFLI $ cylCub cc
