{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Abstract where

import Syntax

-- import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable
import Data.Functor
import Data.Function
import Data.Either
import Data.List
import Control.Applicative
import Control.Monad

import Control.Monad.State.Lazy

import Control.Monad.Writer.Lazy


import qualified Data.Foldable as Foldable

import Data.Functor.Identity

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

-- import Reorientable

import Debug.Trace
import Data.Either (fromRight)

import GHC.Stack.Types ( HasCallStack )

import Data.Aeson

data ClCub b = ClCub {clCub :: FromLI SubFace (OCub b)}
  deriving (Show ,  Functor , Eq)

data CylCub b = CylCub {cylCub :: FromLI SubFace (Maybe (OCub b))}
  deriving (Functor , Eq)

data BdCub b = BdCub {bdCub :: FromLI BdSubFace (OCub b)}
  deriving (Show,Functor , Eq)

instance (Show b) => Show (CylCub b) where
  show x = showMbFLI (cylCub x)

data OCub b =
    CAppl b (ClCub b) [(ClCub b)]
  | Cub Int b (Maybe CellExpr)
  | Hcomp b (Maybe Name) (CylCub b) (ClCub b)
  deriving (Show ,  Functor, Eq)



isHole :: OCub b -> Bool
isHole (Cub _ _ Nothing) = True
isHole _ = False 
-- instance Bifunctor Cub where
--   first f (Cub fc a) = (Cub (fmap (first f) fc) a) 
--   first f (Hcomp b nm si a) = (Hcomp (f b) nm (fmap (first f) si) (first f a))

--   second = fmap


-- TODO : optimize by skipping checking of boundaries
isHoleFreeO :: OCub b -> Bool
isHoleFreeO (Cub _ _ (Nothing)) = False
isHoleFreeO (Cub _ _ (Just _)) = True
isHoleFreeO (CAppl _ x xs) =  
  all (all (isHoleFreeO) . clCub) (x : xs) 
isHoleFreeO (Hcomp _ _ c b) = 
  isHoleFreeO (clInterior b) && all (maybe True isHoleFreeO) (cylCub c) 

isHoleFreeBd :: BdCub b -> Bool
isHoleFreeBd = all isHoleFreeO . bdCub


data AddressPart = AOnCylinder SubFace | AOnBottom SubFace | AArg Int SubFace
  deriving (Show , Eq , Ord,Read)
data Address = Address SubFace [AddressPart]
  deriving (Show , Eq, Ord,Read)


-- this is not only Set of Addresses, it should be quaranted to be valid in some ClCub , and NOT EMPTY!!!
-- TODO :: make constructor private, and enforce this property
newtype CAddress = CAddress {cAddress :: (Set.Set Address)}
  deriving (Show , Eq , Ord)

toAddress = head . Set.toList . cAddress

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


isHcompSideAddr :: Address -> Maybe (Address , SubFace)
isHcompSideAddr (Address a (AOnCylinder sf : sff)) = Just (Address a (sff) , sf) 
isHcompSideAddr _ = Nothing

isHcompSideAddrToAdd :: ClCub () -> Address -> Maybe (Address , SubFace)
isHcompSideAddrToAdd cub addr@(Address y (AOnBottom sf : xs))
   | isFullSF sf = Nothing
   | otherwise =
      case (fmap clInterior $ clCubPick (Address y (xs)) cub) of
        Just (Hcomp () mbn si a) ->
           case (appLI sf (cylCub si)) of
              Nothing -> Just ((Address y (xs)) , sf)
              _ -> Nothing
        _ -> Nothing
         
isHcompSideAddrToAdd _ _ = Nothing

addresedDimPart :: AddressPart -> Int
addresedDimPart (AOnCylinder sf) = subFaceDimEmb sf + 1
addresedDimPart (AOnBottom sf) = subFaceDimEmb sf
addresedDimPart (AArg _ sf) = subFaceDimEmb sf


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

argAddr :: Address -> Int -> Address
argAddr addr@(Address x y) k = Address x (AArg k (fullSF (getDim x)) : y) 


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

instance OfDim (BdCub b) where
  getDim (BdCub fli) = getDim fli


instance OfDim (OCub b) where
  getDim (Cub n _ _) = n
  getDim (CAppl _ f _) = getDim f
  getDim (Hcomp _ _ _ a) = getDim a

instance OfDim Address where
  getDim (Address sf _) = getDim sf

instance OfDim CAddress where
  getDim = getDim . toAddress


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
  | otherwise = error "imposiblem"

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
toFillParentOPart (Cub _ _ _) _ = error "imposible !" -- todo: why ??
  
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

addressClass :: HasCallStack => ClCub b -> Address -> CAddress
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


-- DANGER !!
-- this ingore fillings (it will produce wrong addresses (OR miss some valid superfaces)
-- when there are missing faces in compositions)

addressSuperFaces :: Address -> [Address]
addressSuperFaces (Address sf []) = [ Address ssf [] | ssf <- superSubFaces sf ]
addressSuperFaces (Address sf ((AOnBottom sfA) : xs)) | isFullSF sfA  = []
addressSuperFaces (Address sf ((AOnBottom sfA) : xs)) =
     Address sf (AOnCylinder sfA : xs ) :
   [ Address sf (AOnBottom ssf : xs ) | ssf <- superSubFaces sfA ]

addressSuperFaces (Address sf ((AOnCylinder sfA) : xs)) =
   [ Address sf (AOnCylinder ssf : xs ) | ssf <- filter (not . isFullSF) $ superSubFaces sfA ]

-- first argument is suspected parent
-- TODO : good place to put some sanity check, asi it depends on few assumptions about other functions
isFaceOfClass :: ClCub a -> CAddress -> CAddress -> Maybe Face
isFaceOfClass cub caP ca =
  let a = catMaybes [ mbSubFaceAddr pA chA >>= toFace | chA <- Set.toList $ cAddress ca , pA <- Set.toList $ cAddress caP ]
  in case a of
         [] -> Nothing
         (f : _) -> Just f 

-- wrong until better addressSuperFaces is written
-- commonSuperFace :: ClCub a -> CAddress -> CAddress -> Maybe (CAddress , (Face , Face))
-- commonSuperFace cub ca1 ca2 = 
--   let a = catMaybes [ if (aL1 == aL2 && isValidAddressFor cub aL1) then (Just (addressClass cub aL1)) else Nothing
--                     | aL1 <- concatMap addressSuperFaces $ Set.toList $ cAddress ca1
--                     , aL2 <- concatMap addressSuperFaces $ Set.toList $ cAddress ca2 ]
--   in if ((cAddresedDim ca1) /= (cAddresedDim ca2)) then Nothing else 
--      case a of
--          [] -> Nothing
--          (x : _) -> case (isFaceOfClass cub x ca1 , isFaceOfClass cub x ca2) of
--                          (Just f1 , Just f2) -> Just (x , (f1 , f2))
--                          _ -> error "imposible"


-- commonSuperFace :: ClCub a -> CAddress -> CAddress -> Maybe (CAddress , (Face , Face))
-- commonSuperFace cub ca1 ca2 =
--   let a1 = Set.toList $ cAddress ca1
--       a2 = Set.toList $ cAddress ca2
--       f n addr = return (do
--          (fc1 , _ ) <- uncons $ catMaybes $ fmap (\x -> mbSubFaceAddr addr x >>= toFace ) a1
--          (fc2 , _ ) <- uncons $ catMaybes $ fmap (\x -> mbSubFaceAddr addr x >>= toFace ) a2
--          return (addressClass cub addr , (fc1 , fc2))) 
         
--       fc n addr _ _ = f n addr
--       fn n addr  _ _ _ _ = f n addr

--   in join $ find isJust $ runIdentity $ cubMapTrav fn fc cub


mbSelectFaces :: ClCub a -> Address -> Address -> Address -> Maybe (CAddress , (Face , Face))
mbSelectFaces cub aP a1 a2 =
  let l a = [ fc
          | fc <- genAllLI (addresedDim aP)
          , let addr = addressSubFace aP (toSubFace fc)
          , (isJust $ mbSubAddress addr a) ]
      

  in case (l a1 , l a2) of
       ([f0] , [f1]) -> if f0 /= f1 then Just (addressClass cub aP , (f0 , f1)) else Nothing
       _ -> Nothing
  
                    
  --    addressSuperFaces
-- cAddressSuperFacesCub :: ClCub () -> CAddress -> [ CAddress ]
-- cAddressSuperFacesCub clcub caddr =
--   let addr = head $ Set.toList $ cAddress caddr
--       addrs = addressSuperFaces addr
--   in 


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

cAddressSubFace :: ClCub a -> CAddress -> SubFace -> CAddress
cAddressSubFace cub caddr sf =
  addressClass cub (addressSubFace (head $ Set.toList $ cAddress caddr ) sf)

cAddressVertexes :: ClCub a -> CAddress -> [CAddress]
cAddressVertexes cub caddr =
   cAddressSubFace cub caddr <$>
    (filter (\x -> subFaceDimEmb x == 0) (genAllLI (getDim caddr)))



cAddrWithSubFaces :: ClCub a -> CAddress -> Set.Set CAddress
cAddrWithSubFaces cub x =
  Set.fromList
  [ addressClass cub (addressSubFace (head $ Set.toList $ cAddress x ) sf)
  | sf <- (genAllLI (cAddresedDim x)) ]




bdCubPickSF :: BdSubFace -> BdCub a -> ClCub a
bdCubPickSF bdsfc (BdCub (FromLI n mp)) = ClCub $
   FromLI (n - bdSubFaceCodim bdsfc)
          (mp . sf2BdSubFace . (flip injSubFace (bd2SubFace bdsfc)))

clCubPickSF :: SubFace -> ClCub a -> ClCub a
clCubPickSF sfc (ClCub (FromLI n mp)) = ClCub $
   FromLI (n - subFaceCodim sfc)
          (mp . flip injSubFace sfc)



clInterior :: ClCub a -> OCub a
clInterior (ClCub (FromLI n mp)) = mp (fullSF n)

clBoundary :: ClCub a -> BdCub a
clBoundary x =
   BdCub $ proMap bd2SubFace (clCub x)

uncurryCl :: (BdCub b -> OCub b -> a) -> (ClCub b -> a)
uncurryCl f x = f (clBoundary x) (clInterior x)

-- todo :: finish work on less naive , more efficient implementation
clCubPick :: forall a. Address -> ClCub a -> Maybe (ClCub a)
clCubPick addr = 
    h
  . foldl g []
  . cubMap (\_ addr b mbn cyl btm -> (addr , Hcomp b mbn cyl btm) )
           (\n addr b mbce -> (addr , Cub n b mbce ))
           undefined

  where
    g l (addr' , ocub) =
      case (mbSubFaceAddr addr addr') of
        Nothing -> l
        Just sf -> (sf , ocub) : l

    h :: [(SubFace, OCub a)] -> Maybe (ClCub a)
    h x | any (isFullSF . fst) x && lengthOK = 
               Just
             $ ClCub
             $ FromLI (addresedDim addr)
             $ ((maybe (error "clCubPickFail")) id . (flip Map.lookup $ Map.fromList x) )
        | any (isFullSF . fst) x = error
                 $ "clCubPickFail2: " ++ (show (length x) ++ " " ++ show (cardLI (Never :: Never SubFace ) (addresedDim addr) )  )
        | otherwise = 
            Nothing

        where
          lengthOK = ((length x) == (cardLI (Never :: Never SubFace ) (addresedDim addr) )  )


-- TODO , use oCubPick ? compare performance?
isHoleClass :: ClCub a -> CAddress -> Bool 
isHoleClass cl ca =
   maybe False (isHole . clInterior) $ (clCubPick (head $ Set.toList (cAddress ca)) cl)

                     
      -- ((Map.!) $ Map.fromList x)
    
-- clCubPick (Address sfc addr@(_ : _)) x@(ClCub fli)
--    | isFullSF sfc =
--       case (reverse addr , appLI sfc fli)  of
--         (AOnCylinder sf : xs , Hcomp _ _ cy y) -> 
--            case appLI sf (cylCub cy) of
--              Just y -> undefined --todo --first (1 +) (oCubPick (reverse xs) y)
--              Nothing -> Left 0
--         (AOnBottom sf : xs , Hcomp _ _ _ y) ->
--             first (1 +) (clCubPick (Address sf (reverse xs)) y)
--         (_ : _ , _) ->
--             Left 0
--         ([] , _) -> Right x
--    | otherwise =
--        first (1 +)
--        $ clCubPick (Address (fullSF (subFaceDimEmb sfc)) addr) (clCubPickSF sfc x)

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

--TODO write some docs about property of CAddress and "beeing subface" relation in both Address and CAddress

addressClassSuperCellsClassLeafs :: ClCub a -> CAddress -> Set.Set CAddress
addressClassSuperCellsClassLeafs cl x =  
    foldl Set.union Set.empty
  $ Set.map (Set.fromList . (fmap $ addressClass cl) . (onlyLeafs . addressSuperCells cl)) $ cAddress x

addressClassSuperHoles :: ClCub a -> CAddress -> Set.Set CAddress 
addressClassSuperHoles cl =    
   (Set.filter (isHoleClass cl )) .
   addressClassSuperCellsClassLeafs cl

-- (filter ((\x -> True) . (head . Set.toList . cAddress))) . 

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

clDegenerate :: Int -> ClCub () -> ClCub () 
clDegenerate k (ClCub (FromLI n f)) = 
  let f' x@(SubFace _ sfm0) =
        case (degenElim k x) of
          DCIns sf@(SubFace _ sfm) -> oDegenerate (fromJust $ punchOutMany (Map.keysSet sfm0) k) (f sf)
          DCEnd _ sf -> f sf
  in ClCub (FromLI (n + 1) f')

clDegenerateMany :: ClCub () -> [ Int ] -> ClCub () 
clDegenerateMany = foldr clDegenerate
  
oDegenerate :: Int -> OCub () -> OCub () 
oDegenerate k (Cub n () x) = Cub (n + 1) () $ fmap (degenerateCellExpr k) x
oDegenerate k (Hcomp () mbN (CylCub (FromLI n si)) btm) = 
  let si' x@(SubFace _ sfm0) =
        case (degenElim k x) of
          DCIns sf@(SubFace _ sfm) -> fmap
                      (oDegenerate (fromJust $ punchOutMany (Map.keysSet sfm0) k))
                      (si sf )
          DCEnd _ sf -> si sf
      
  in Hcomp () mbN (CylCub (FromLI (n + 1) si')) (clDegenerate k btm)


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
      cubMap' nf cf undefined cl

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
      cubMap' nf cf undefined cl

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
          insQ = Prelude.max (maximumAlways (-1) (fmap ((maybe (-1) id ) . fst) cy))
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
          -> (Address -> b -> ClCub b -> [ClCub b] -> a bb)
                 -> ClCub b
                 -> a (ClCub bb)
cubMapTrav g f u (ClCub xx) = ClCub <$>
   traverseMapLI (\sf -> cm (Address sf [])) xx

  where
     cm :: (Monad a) => Address -> OCub b -> a (OCub bb)

     cm addr@(Address sf0 tla) (CAppl b x xs) =
       
          do let n = (getDim x)
                 hh (i,y) = ClCub
                      <$> traverseMapLI (\sf -> (cm (Address sf0 (AArg i sf : tla)))) (clCub y)
             b2 <- u addr b x xs
             h0 <- hh (0 , x)
             hT <- mapM hh (zip [1..] xs)
             return $ CAppl b2 h0 hT

     
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
          -> (Address -> b -> ClCub bb -> [ClCub bb] -> a (bb))
                 -> ClCub b
                 -> a (ClCub bb)
cubMapTrav' g f u (ClCub xx) = ClCub <$>
   traverseMapLI (\sf -> cm (Address sf [])) xx

  where
     cm :: (Monad a) => Address -> OCub b -> a (OCub bb)
     cm addr@(Address sf0 tla) (CAppl b x xs) =

       do let n = (getDim x)
              hh (i,y) = ClCub
                   <$> traverseMapLI (\sf -> (cm (Address sf0 (AArg i sf : tla)))) (clCub y)
          
          h0 <- hh (0 , x)
          hT <- mapM hh (zip [1..] xs)
          b2 <- u addr b h0 hT
          return $ CAppl b2 h0 hT

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
          -> (Address -> BdCub b -> b -> ClCub b -> [ClCub b] -> a bb)
                 -> ClCub b
                 -> a (ClCub bb)
cubMapTravWithB g f u xxx@(ClCub xx) = ClCub <$>
   traverseMapLI (\sf _ -> uncurryCl (cm (Address sf [])) (clCubPickSF sf xxx)) xx

  where
     cm :: (Monad a) => Address -> BdCub b -> OCub b -> a (OCub bb)

     cm addr@(Address sf0 tla) bd (CAppl b x xs) =
       
          do let n = (getDim x)
                 hh (i,y) =
                     ClCub
                      <$> traverseMapLI (\sf _ ->
                                                  uncurryCl (cm (Address sf0 (AArg i sf : tla)))
                                                  (clCubPickSF sf y)  ) (clCub y)
             b2 <- u addr bd b x xs
             h0 <- hh (0 , x)
             hT <- mapM hh (zip [1..] xs)
             return $ CAppl b2 h0 hT
     
     cm addr@(Address _ tla) bd (Cub n b mbc) =
          do b2 <- f n addr bd b mbc
             return $ Cub n b2 mbc



     cm addr@(Address sf0 tla) bd ocub@(Hcomp b mbNm cyl btm) =
          do b2 <- g (getDim ocub) addr bd b mbNm cyl btm
             let cylBd :: SubFace -> BdCub b
                 cylBd sf = clBoundary $ ClCub $ elimSubFaceSideLI
                          (clCub (clCubPickSF sf btm))
                          (clCub (bdCubPickSF (sf2BdSubFace sf) bd))
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
          -> (Address -> BdCub b -> b -> ClCub b -> [ClCub b] -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMapWithB f g u = runIdentity . cubMapTravWithB (dot7 Identity f) (dot5 Identity  g)
     (dot5 Identity u)
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
          -> (Address -> b -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMap f g u = runIdentity . cubMapTrav (dot6 Identity f) (dot4 Identity  g)
  (\a x _ _ -> Identity (u a x))

cubMap' :: (Int -> Address -> b -> Maybe Name -> CylCub bb -> ClCub bb -> bb)
          -> (Int -> Address -> b -> Maybe CellExpr -> bb)
         -> (Address -> b -> bb)
                 -> ClCub b
                 -> ClCub bb
cubMap' f g u = runIdentity . cubMapTrav' (dot6 Identity f) (dot4 Identity  g)
  (\a x _ _ -> Identity (u a x))

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
cubMapWAddr f = cubMap (\_ addr b _ _ _ -> f addr b) (\_ addr b _ -> f addr b) f

clClearAllCells :: ClCub [b] -> ClCub [b]
clClearAllCells = cubMapWAddr (\_ _ -> [])


cubMapMayReplace  :: forall a b . (Monad a) =>  (Int -> Address -> OCub b -> a (Maybe (OCub b)))
                 -> ClCub b
                 -> a (ClCub b)
cubMapMayReplace f (ClCub xx) = ClCub <$>
   traverseMapLI (\sf -> cmm (Address sf [])) xx

  where
    cmm :: Address
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
                       (\sf -> maybe (pure Nothing)
                                     (fmap Just . cmm (Address sf0 (AOnCylinder sf : tla))))
                        (cylCub cyl)
                   btm2 <- ClCub <$> traverseMapLI
                              (\sf -> cmm (Address sf0 (AOnBottom sf : tla))) (clCub btm)
                   return (Hcomp b mbNm cyl2 btm2)

             Cub {} -> return x


-- -- variant of cubMap constant on node data
cubMapOld = cubMap (const $ const $ \b -> const $ const $ const b) undefined

cubMapOldBoth f = cubMap (\n addr b _ _ _ -> f n addr b) (\n addr b _ -> f n addr b)
   undefined 




travOldBoth :: forall a b b2 . (Monad a) =>
                 (Int -> Address -> b -> a b2)
                 -> ClCub b -> a (ClCub b2)
travOldBoth f = cubMapTrav' (\n addr b _ _ _ -> f n addr b) (\n addr b _ -> f n addr b) undefined


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
  foldMap f (CAppl b x xs) =  f b <> foldMap f x <> (mconcat $ fmap (foldMap f ) xs)

instance Foldable ClCub where
  foldMap f (ClCub w) = foldMap (foldMap f) w

instance Foldable BdCub where
  foldMap f (BdCub w) = foldMap (foldMap f) w


instance Foldable CylCub where
  foldMap f (CylCub w) = mconcat $ fmap (foldMap f ) $ catMaybes $ toListFLI w

-- untested !!!!
isHoleFree :: ClCub a -> Bool
isHoleFree x = execState (cubMapTrav (\_ _ _ _ _ _  -> return ()) (\_ _ _ z -> modify (&& (isJust z) )) undefined  x)  True

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




-- clCubFace :: forall b. Face -> ClCub b -> ClCub b
-- clCubFace fc = ClCub . ccf . clCub

--   where
--     ccf (FromLI 0 _) = error "attempt to take face of 0-dimensional cub"
--     ccf (FromLI n mp) =
--       FromLI (n - 1) (mp . flip injSubFace (toSubFace fc))

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
                         (toOCub (env , addSFConstraintToContext sfc ct))
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
toOCub (env , ct) (Generic _) = Cub (getDim ct) () Nothing

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


fromBdCubTyMb :: (Env , Context) -> BdCub () -> Maybe CType
fromBdCubTyMb (e , c) x | isHoleFreeBd x = 
        
    Just $ CType (BType 0) $ [ 
                              mapBoth
                              (\b ->
                                 let sf = (toBdSubFace (Face (getDim x) (i , b)))
                                     c' = traceShowId $ addSFConstraintToContext (bd2SubFace sf) c                                     
                                     y = fromOCub (e , c') $ appLI sf  (bdCub x)
                                 in deContextualizeFace c' y
                              )  
                              (False , True)
                           | i <- [0..((getDim x)-1)] ] 
                       

 

                       | otherwise = Nothing


-- clCellType :: (Env , Context) -> BType -> Address -> ClCub () -> CType
-- clCellType eee@(ee , ctx) btype addr clcub =
--   CType btype fcs

--     where
--       n = getDim clcub

--       f fc =
--         let sf = toSubFace fc
--             addr' = addressSubFace addr sf
--             cubFc = void $ fromMaybe (error "clCellType-error") $ clCubPick addr' clcub
--             ctx' = contextAt ctx addr clcub
--         in deContextualizeFace ctx' (fromClCub (ee , ctx') cubFc)
          
--       fcs = [ (f $ Face n (k , False)  , f $ Face n (k , True))
--              | k <- range n ] 


-- deContextualizeAt :: (Env , Context) -> ClCub () -> CAddress -> LExpr
-- deContextualizeAt (ee , ctx) cub caddr =
--   let addr = toAddress caddr
--       ctx' = contextAt ctx addr cub
--   in deContextualizeFace ctx' (fromClCub (ee , ctx') (fromJust $ clCubPick addr cub))


-- getFrom




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



-- TODO :: this is not yet well tested, but should be ok
contextAtIns :: Context -> [AddressPart] -> OCub b -> Maybe Context
contextAtIns ctx [] _ = Just ctx
contextAtIns ctx addr (Hcomp _ nam si a) = 
     case reverse addr of
        AOnCylinder sf : xs ->
           case appLI sf (cylCub si) of
             Just y ->
               contextAtIns (addSFConstraintToContext sf (addDimToContext ctx (nam))) (reverse xs) y
             Nothing -> Nothing
        AOnBottom sf : xs -> contextAtSafe ctx (Address sf (reverse xs)) a 
        _ -> error "imposible, this case should be handled in ther firt clause"
contextAtIns _ (_:_) (Cub _ _ _) = error "imposible" -- todo: why?

contextAtSafe :: Context -> Address -> ClCub b -> Maybe Context
contextAtSafe ctx (Address sf xs) y =
  contextAtIns (addSFConstraintToContext sf ctx) xs (clInterior $ clCubPickSF sf y)


contextAt :: Context -> Address -> ClCub b -> Context
contextAt ctx a@(Address sf xs) y =
 fromJust $ contextAtIns (addSFConstraintToContext sf ctx) xs (clInterior $ clCubPickSF sf y)
                                
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

---- unify
-- TODO : investigate performance , be sure to recognise that unification fails AEAP
-- TODO : add tracing of unification, merging where left,right,both and no term is defined (to use with Bool in ExprTransform)

data PreUnificationResult a1 a2  =
     Agreement a1 a2
   | Val1 a1 (Maybe a2)
   | Val2 (Maybe a1) a2
   | Mixed a1 a2
   | Conflict (OCub a1) (OCub a2) 
 deriving (Show)


fromVal1 :: PreUnificationResult a1 a2 -> a1
fromVal1 (Val1 a1 _) = a1
fromVal1 _ = undefined

fromAgreement :: PreUnificationResult a1 a2 -> (a1 , a2)
fromAgreement (Agreement a1 a2) = (a1 , a2)
fromAgreement _ = undefined



instance Bifunctor (PreUnificationResult) where
  bimap f g x =
    case x of
     Agreement a1 a2 -> Agreement (f a1) (g a2) 
     Val1 a1 mba2 -> Val1 (f a1) (fmap g mba2) 
     Val2 mba1 a2 -> Val2 (fmap f mba1) (g a2)
     Mixed a1 a2 -> Mixed (f a1) (g a2)
     Conflict oca1 oca2 -> Conflict (fmap f oca1) (fmap g oca2) 

  
isAgreementQ :: PreUnificationResult a1 a2 -> Bool
isAgreementQ (Agreement _ _) = True
isAgreementQ _ = False

isConflictQ :: PreUnificationResult a1 a2 -> Bool
isConflictQ (Conflict _ _) = True
isConflictQ _ = False


isAgreementOCub :: OCub (PreUnificationResult a1 a2) -> Bool
isAgreementOCub = isAgreementQ . oCubPickTopLevelData

hasConflict :: (Foldable t) => t (PreUnificationResult a1 a2) -> Bool
hasConflict = any isConflictQ

isAgreementClCub :: ClCub (PreUnificationResult a1 a2) -> Bool
isAgreementClCub = all isAgreementOCub . clCub



-- data StrictUnificationResult a1 a2  =
--      SURAgreement a1 a2
--    | SURVal1 a1 (Maybe a2)
--    | SURVal2 (Maybe a1) a2
--    | SURMixed a1 a2

--  deriving (Eq, Show)

-- tryExtractUnifyStrict :: (Functor t , Foldable t) =>
--                            t (PreUnificationResult a1 a2)
--                             -> Maybe (t (StrictUnificationResult a1 a2))
-- tryExtractUnifyStrict x
--   | hasConflict x = Nothing
--   | otherwise = Just (fmap f x)
--      where
--       f (Agreement a1 a2) = SURAgreement a1 a2
--       f (Val1 a1 mba2) = SURVal1 a1 mba2 
--       f (Val2 mba1 a2) = SURVal2 mba1 a2
--       f (Mixed a1 a2) = SURMixed a1 a2 
--       f (Conflict _ _) = error "imposible"


  

data PreUnificationResultCyl a1 a2  =
       CylUAgreement (CylCub (a1 , a2))
     | CylUMixed (CylCub (PreUnificationResult a1 a2))
     | CylUConflict

instance Bifunctor (PreUnificationResultCyl) where
  bimap f g = \case
       CylUAgreement x -> CylUAgreement (fmap (bimap f g) x)
       CylUMixed x -> CylUMixed (fmap (bimap f g) x) 
       CylUConflict -> CylUConflict

     

instance Show (PreUnificationResultCyl a1 a2) where
  show = \case
       CylUAgreement x -> "CylUAgreement" 
       CylUMixed x -> "CylUMixed"
       CylUConflict -> "CylUConflict"



preUnifyCylCub :: CylCub a1 -> CylCub a2 -> PreUnificationResultCyl a1 a2
preUnifyCylCub (CylCub (FromLI aN _)) (CylCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
preUnifyCylCub (CylCub f1@(FromLI n _)) (CylCub f2@(FromLI _ _)) = 
 let m1 = Map.filter isJust $ toMapFLI f1 
     m2 = Map.filter isJust $ toMapFLI f2
     
 in if (Map.keysSet m1 /= Map.keysSet m2)
    then CylUConflict
    else let (l1 , l2) = ((map (second fromJust)) $ (Map.toAscList m1)
                         ,(map (second fromJust)) $ (Map.toAscList m2))
             f (sf,v1) (_,v2) =
                 (sf , preUnifyOCub v1 v2)
             zipped = fromMapFLI n (Map.fromList (zipWith f l1 l2))
             aggrees =
                all (fromMaybe True . (fmap isAgreementOCub))
                  zipped
         in if aggrees
            then (CylUAgreement (fmap (\(Agreement a1 a2) -> (a1,a2)) (CylCub zipped))) 
            else (CylUMixed (CylCub zipped))
            
    
preUnifyOCub :: OCub a1 -> OCub a2 -> (OCub (PreUnificationResult a1 a2))

preUnifyOCub a b | isHole a || isHole b =
 case (a , b) of
    (Cub n x1 Nothing , Cub _ x2 Nothing) ->
        Cub n (Agreement x1 x2) Nothing
    (Cub _ x Nothing , _) ->
        g (Val2 Nothing) (Val2 (Just x)) b
    (_ , Cub _ x Nothing) ->
        g (flip Val1 Nothing) (flip Val1 (Just x)) a
    (_) -> error "imposible" -- gurard checks if one of args is hole

  where

    g _ f' (Cub n x2 (Just cE)) = (Cub n (f' x2) (Just cE))
    g f f' (Hcomp x2 nameA sidesA btmA) =
       (Hcomp (f' x2) nameA
        (fmap f (sidesA))
        (fmap f (btmA))
       )
    g _ _ (Cub _ _ Nothing) = error "imposible" -- this would trigger first case above

preUnifyOCub a@(Cub n a1 x) (Cub _ a2 y) | x == y =
       (Cub n (Agreement a1 a2) x)                                               
preUnifyOCub e1@(Hcomp x1 nameA sidesA btmA) e2@(Hcomp x2 nameB sidesB btmB) =
  let btmU = preUnifyClCub btmA btmB
      sidesU = preUnifyCylCub sidesA sidesB
      nameU = maybe nameA Just nameB 
  in case (sidesU , isAgreementClCub btmU) of
        (CylUAgreement x , True) ->
            Hcomp (Agreement x1 x2)
                  nameU
                  (fmap (uncurry Agreement) x)
                  btmU
        (CylUConflict , _) ->
           Cub (getDim e1) (Conflict e1 e2 ) Nothing
        (CylUMixed x , _) ->
            Hcomp (Mixed x1 x2)
                  nameU
                  x
                  btmU
        (_, False) -> Cub (getDim e1) (Conflict e1 e2 ) Nothing  
     
preUnifyOCub e1 e2 =
   Cub (getDim e1) (Conflict e1 e2 ) Nothing
  
preUnifyClCub :: ClCub a1 -> ClCub a2 -> ClCub (PreUnificationResult a1 a2)
preUnifyClCub (ClCub (FromLI aN _)) (ClCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
preUnifyClCub (ClCub (FromLI n f1)) (ClCub (FromLI _ f2)) =
  ClCub (FromLI n (\sf -> preUnifyOCub (f1 sf) (f2 sf)))

preUnifyBdCub :: BdCub a1 -> BdCub a2 -> BdCub (PreUnificationResult a1 a2)
preUnifyBdCub (BdCub (FromLI aN _)) (BdCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
preUnifyBdCub (BdCub (FromLI n f1)) (BdCub (FromLI _ f2)) =
  BdCub (FromLI n (\sf -> preUnifyOCub (f1 sf) (f2 sf)))


-- unifyClCub :: ClCub a1 -> ClCub a2 -> ClCub (PreUnificationResult a1 a2)
-- unifyClCub e1 e2 =
--   let preU = preUnifyClCub e1 e2
--       clss = getAllAddrClasses preU
--   in undefined


  
  -- ClCub <$> sequence (FromLI n (\sf -> preUnifyOCub (a sf) (b sf)))

  
-- preUnifyCylCub :: CylCub () -> CylCub () -> Either () (CylCub ())
-- preUnifyCylCub (CylCub (FromLI aN _)) (CylCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
-- preUnifyCylCub (CylCub (FromLI n a)) (CylCub (FromLI _ b)) =
--   CylCub <$> sequence (FromLI n
--     (\sf -> case (a sf , b sf) of
--               (Just aO , Just bO) -> Just <$> preUnifyOCub aO bO 
--               (Nothing , Nothing) -> Right Nothing
--               _ -> Left ()
--                                      ))

-- preUnifyOCub :: OCub () -> OCub () -> Either () (OCub ())
-- preUnifyOCub a b | isHole a = Right b
-- preUnifyOCub a b | isHole b = Right a
-- preUnifyOCub a@(Cub _ _ (Just x)) (Cub _ _ (Just y)) | x == y = Right a
-- preUnifyOCub (Hcomp _ nameA sidesA btmA) (Hcomp _ nameB sidesB btmB) = do
--   btmU <- preUnifyClCub btmA btmB
--   sidesU <- preUnifyCylCub sidesA sidesB
--   let nameU = maybe nameA Just nameB 
--   return $ Hcomp () nameU sidesU btmU

-- preUnifyOCub _ _ = Left () 
  
-- preUnifyClCub :: ClCub () -> ClCub () -> Either () (ClCub ())
-- preUnifyClCub (ClCub (FromLI aN _)) (ClCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
-- preUnifyClCub (ClCub (FromLI n a)) (ClCub (FromLI _ b)) =
--   ClCub <$> sequence (FromLI n (\sf -> preUnifyOCub (a sf) (b sf)))



-- unifyOCub :: OCub a1 -> OCub a2 -> OCub (PreUnificationResult a1 a2)
-- unifyOCub x cub =
--   preUnifyOCub x cub




data PreUnifySchema a b =
  PreUnifySchema
  { usPreUnify :: OCub a -> OCub a -> Either () (CylCub b) 
  }


preUnifyCylCubSchema :: PreUnifySchema a b -> CylCub a -> CylCub a -> Either () (CylCub b)
preUnifyCylCubSchema = undefined
-- preUnifyCylCub (CylCub (FromLI aN _)) (CylCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
-- preUnifyCylCub (CylCub (FromLI n a)) (CylCub (FromLI _ b)) =
--   CylCub <$> sequence (FromLI n
--     (\sf -> case (a sf , b sf) of
--               (Just aO , Just bO) -> Just <$> preUnifyOCub aO bO 
--               (Nothing , Nothing) -> Right Nothing
--               _ -> Left ()
--                                      ))

preUnifyOCubSchema :: PreUnifySchema a b -> OCub a -> OCub a -> Either () (OCub b)
preUnifyOCubSchema = undefined
-- preUnifyOCub a b | isHole a = Right b
-- preUnifyOCub a b | isHole b = Right a
-- preUnifyOCub a@(Cub _ _ (Just x)) (Cub _ _ (Just y)) | x == y = Right a
-- preUnifyOCub (Hcomp _ nameA sidesA btmA) (Hcomp _ nameB sidesB btmB) = do
--   btmU <- preUnifyClCub btmA btmB
--   sidesU <- preUnifyCylCub sidesA sidesB
--   let nameU = maybe nameA Just nameB 
--   return $ Hcomp () nameU sidesU btmU

-- preUnifyOCub _ _ = Left () 
  
preUnifyClCubSchema :: PreUnifySchema a b -> ClCub a -> ClCub a -> Either () (ClCub b)
preUnifyClCubSchema = undefined
-- preUnifyClCub (ClCub (FromLI aN _)) (ClCub (FromLI bN _)) | aN /= bN = error "not maching dim of cubs"
-- preUnifyClCub (ClCub (FromLI n a)) (ClCub (FromLI _ b)) =
--   ClCub <$> sequence (FromLI n (\sf -> preUnifyOCub (a sf) (b sf)))





---- give

giveOptions :: Context -> VarIndex -> [ CellExpr ]
giveOptions _ x = [ CellExpr x [] , CellExpr x [] , CellExpr x [] ]


-- this DO NOT checks if given CellExpr "fits" into CAddress,
-- this is just helper to (1) extract proper context (2) evaluate subfaces with the help of toClCub
toClCubAt :: Env -> Context -> ClCub a -> CAddress -> CellExpr -> ClCub ()
toClCubAt env ctx clcub caddr ce =
   let addr = head $ Set.toList $ cAddress caddr
       ctx' = contextAt ctx addr clcub
       n = addresedDim addr
   in toClCub (env , ctx') (fromOCub (env , ctx') (Cub n () (Just ce)))


-- IExpr here are made of DimIndexes

substDimsOCub :: Int -> [IExpr] -> OCub () -> OCub ()
substDimsOCub newN ies ocub =
  case ocub of
    Cub oldN () Nothing ->
       Cub newN () Nothing
    Cub oldN () (Just (CellExpr vi tl)) ->
       Cub newN () (Just (CellExpr vi $ fmap (substIExpr' ies) tl))
    Hcomp () mbn pa x ->
      let pa' = substDimsCylCub newN ies pa
          x' = substDimsClCub newN (fmap Right ies) x 
      in Hcomp () mbn pa' x'
  
substDimsCylCub :: Int -> [IExpr] -> CylCub () -> CylCub ()
substDimsCylCub newN ies (CylCub fli@(FromLI oldN _)) =
  let m = fmap (second fromJust) $ filter (isJust . snd ) $ Map.toList $ toMapFLI fli
      m' = [ (sf' , e')  
           | (sf , e) <- m
           , sf' <- substSubFace newN ies sf
           , let ies' = (fmap snd $ filter (isNothing . fst) $ safeZip (toListLI sf) ies)
                 (endsIE , ies'') = partitionEithers $ fmap (projIExpr' sf') ies'
           -- , null endsIE <- this should always be True, but maybe be usefull for debuging purposes as sanity check
           , let e' = substDimsOCub (subFaceDimEmb sf' + 1) (ies'' ++ [dim $ subFaceDimEmb sf']) e
           ]
  in CylCub (fromMapFLI newN $ Map.fromList m')

substDimsClCub :: Int -> [(Either Bool IExpr)] -> ClCub () -> ClCub ()
substDimsClCub newN ies cub
   | getDim cub /= length ies = error "not enough IExprs , to substitute all free varaibles"
   | otherwise = 
  let oldN = getDim cub
      (FromLI _ f0) = clCub cub
      f sf = 
         let ies' = fmap (join . second (projIExpr' sf)) ies
             sf' = fromListLI $ fmap (either Just (\_ -> Nothing)) ies'             
         in substDimsOCub (subFaceDimEmb sf) (rights ies') (f0 sf')

  in ClCub $ FromLI newN f


clDegenerateTest :: Int -> ClCub () -> ClCub () 
clDegenerateTest k cub =
  let n = getDim cub
      
  in substDimsClCub (n + 1) [ Right (dim (punchIn k i)) | i <- range n ] cub

cornerTail :: Face -> Face -> [IExpr]
cornerTail (Face n (k0 , b0)) (Face n' (k1 , b1))
                   | n /= n' = error "bad dims"
                   | otherwise =
                          if k0 == k1
                          then error "faces should have only one common edge"
                          else let ie1 = if (b0 == b1) then dim k0 else neg (dim k0)
                                   op = if b1 then Syntax.min else Syntax.max
                                   ie01 = op ie1 (dim k1)
                               in [ let i' = punchIn k0 i in
                                    if i' == k1
                                    then ie01
                                    else dim i'
                                  | i <- range (n - 1)]

negateDim :: Int -> ClCub () -> ClCub ()
negateDim k x = substDimsClCub (getDim x) [ Right $ if i == k then neg (dim i) else dim i | i <- range (getDim x) ] x 


-- cAddress must point to "proper hole" - hole of which all subfaces are holes
-- otherwsie function will encounter error

substAtHole :: forall a1 a2. ClCub a1 -> CAddress -> ClCub a2 -> ClCub (Maybe a1 , Maybe a2)
substAtHole x caddr cub = 
  foldl ff (fmap (( , Nothing ) . Just ) x) (allSubFaces holeDim)

  where
    holeDim = addresedDim $ head $ Set.toList (cAddress caddr)

    ff :: (ClCub (Maybe a1 , Maybe a2))
             -> SubFace -> (ClCub (Maybe a1 , Maybe a2))    
    ff y sf =
      let caddr' = cAddressSubFace x caddr sf
          cubO = appLI sf (clCub cub)
          f n addr x = Identity $
            if Set.member addr (cAddress caddr')
            then Just $
              (if isHole x then
              (if (isFullSF sf)
               then (fmap (( Nothing ,) . Just) cubO)
               else ( fmap (( (fst $ oCubPickTopLevelData x) ,) . Just) cubO) 
              ) else (error "caddr points does not point at a proper hole!" ) )
            else Nothing
      in runIdentity (cubMapMayReplace f y)


-- cAddress must point to "hole" - hole of which all subfaces are holes
-- otherwsie function will encounter error
-- this function can be used ONLY when <cub> fits EXACTLY inside address
-- it will only subsitute indicated addres, it will NOT subsitute its subfaces (it assumes that they are already subsituted)

-- this function is Unsafe, becpuse it will not even throw error if <cub> do not fits, it will simply output ill produced term

clearCell :: CAddress -> ClCub a -> ClCub (Bool , a)
clearCell caddr clcub =
  let tracked = traceConstraints clcub (Set.singleton caddr)
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Identity Nothing 
          y@(Just k , a) -> Identity $ Just $ (Cub n y Nothing) 
  in (fmap (first isJust)) $ runIdentity $ cubMapMayReplace f tracked

clearCellWithFreeSF :: CAddress -> ClCub a -> ClCub (Bool , a)
clearCellWithFreeSF caddr clcub' =
  let clcub =  clearCell caddr clcub'
      tracked = traceConstraints clcub (Set.filter (isFreeAddressClass clcub ) $ cAddrWithSubFaces clcub caddr)
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Identity Nothing 
          y@(Just k , _) -> Identity $ Just $ (Cub n y Nothing) 
  in (fmap (\(b , (b' , a)) -> ((isJust b) || b' , a))) $ runIdentity $ cubMapMayReplace f tracked


-- this is UNSAFE!!! -- may produce ill terms!
substInside :: forall a1 a2. OCub a2 -> CAddress -> ClCub a1 -> (ClCub (Maybe a1 , Maybe a2))
substInside oCub caddr x = 
  ff (fmap (( , Nothing ) . Just ) x)

  where
    holeDim = addresedDim $ head $ Set.toList (cAddress caddr)

    ff :: ClCub (Maybe a1 , Maybe a2) -> (ClCub (Maybe a1 , Maybe a2))    
    ff y =
      let f n addr x = Identity $ 
            if Set.member addr (cAddress caddr)
            then (Just $  fmap (( Nothing ,) . Just) oCub)               
            else Nothing
          
      in runIdentity (cubMapMayReplace f y)


substInsideHole :: forall a1 a2. ClCub a1 -> CAddress -> OCub a2 -> Maybe (ClCub (Maybe a1 , Maybe a2))
substInsideHole x caddr oCub = 
  ff (fmap (( , Nothing ) . Just ) x)

  where
    holeDim = addresedDim $ head $ Set.toList (cAddress caddr)

    ff :: ClCub (Maybe a1 , Maybe a2) -> Maybe (ClCub (Maybe a1 , Maybe a2))    
    ff y =
      let f n addr x = 
            if Set.member addr (cAddress caddr)
            then 
              (if isHole x then
              (Just (Just $  fmap (( Nothing ,) . Just) oCub))
               else (Nothing) )
            else Just Nothing
          
      in (cubMapMayReplace f y)


substInsideHoleUnsafe :: forall a1 a2. ClCub a1 -> CAddress -> OCub a2 -> ClCub (Maybe a1 , Maybe a2)
substInsideHoleUnsafe x caddr oCub = 
  fromJust $ substInsideHole x caddr oCub



type Address2PointMap = Map.Map Address (Set.Set Address)


mkAddress2PointMap :: forall a1 . ClCub a1 -> Address2PointMap
mkAddress2PointMap  cub =  
    execWriter (cubMapTravWithB f g undefined wb)

  where
    wb :: ClCub (Address)
    wb = cubMapWAddr (\a _ -> a) cub

    h n addr bd = 
       tell
         $ Map.singleton addr
         $ Set.fromList $ if n ==0 then [addr] else
                             (  ( oCubPickTopLevelData . (flip appLI (bdCub bd)) . subsetToBdSubFace) <$> (genAllLI n))
                 

    f n addr bd _ _ _ _ = h n addr bd
       
    
    g n addr bd _ _ = h n addr bd 

    
  

pointSelectStep :: ClCub a -> Address2PointMap -> Address -> Address -> (Maybe Address)
pointSelectStep cub a2pm addrSel addrPt =
   let q = a2pm Map.! addrSel
       z = Map.filter
              (\s -> Set.member addrPt s && Set.isProperSubsetOf q s)
              a2pm
   in listToMaybe $ sortOn addresedDim $ Map.keys z


countIndependentHoles :: ClCub a -> Int
countIndependentHoles cub = undefined


