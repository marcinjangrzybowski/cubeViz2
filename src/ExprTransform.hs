{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExprTransform where

import Syntax

-- import Drawing.Base

import Data.Maybe
import Data.Either
import Data.Bifunctor
import Data.Traversable
import Data.Functor
import Data.Foldable

import Control.Applicative

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

import Abstract

import Debug.Trace

import Control.Monad.State.Strict


data ClearCellRegime =
    OnlyInterior
  | WithFreeSubFaces
 deriving (Eq)


-- data ConstraintsOverrideStrategy =
--      COSClearWithConstrained
--    | COSClearNecessary
--    | COSInflate
--   deriving (Eq)

-- data ConstraintsOverrideRegime = NoConstraintsOverride | ConstraintsOverride ConstraintsOverrideStrategy

data RemoveSideRegime = RSRRemoveLeavesAlso | RSRKeepLeaves
   
data CubTransformation =
     ClearCell CAddress ClearCellRegime
   | SplitCell CAddress
   | FillHole CAddress (ClCub ())
   | RemoveSide CAddress (SubFace , RemoveSideRegime)
   | AddSide CAddress SubFace
   -- | ReplaceAt Address (ClCub ())
   -- | RemoveCell Address
   -- | RemoveCellLeaveFaces Address
   -- | AddSubFace (Address , SubFace)
   -- | MapAt Address (ClCub () -> Either String (ClCub ()))
  -- deriving (Show)

-- TODO :: transofrmations for filling - automaticly recognises holes in faces of compositions  which can be removed


data FillHoleConflict =
  FillHoleConflict
  { fhcOuter :: ClCub ()
  , fhcHoleCAddress :: CAddress
  , fhcCandidate :: ClCub ()
  , fhcConflictingAddresses :: Set.Set (Address , Address)
  }
 deriving (Show)

data HoleConflictResolutionStrategy =
    ClearOutwards
  | ClearInwards
  | InflateInwards
  | InflateOutwards
  deriving (Enum, Show, Bounded, Eq)

data CubTransformationError =
     CubTransformationError String
   | CubTransformationConflict FillHoleConflict

 


instance Show CubTransformationError where
  show (CubTransformationError x) = x
  show (CubTransformationConflict x) = show x


-- rodzaje nadpisywania adresu tylko dla hole :
-- -- tylko hole w inerior
-- -- hole w interior + wolne scinay
-- rodzaje nadpisywania adresu

fillHole :: ClCub () -> CAddress -> ClCub () -> Either FillHoleConflict (ClCub Bool)
fillHole x caddr cub =
    fillAllSubFaces
      
  where

    holeDim = addresedDim $ head $ Set.toList (cAddress caddr)



    -- TODO : boleans here are set to True, even if no transformation is aplied (becouse term is already there)
    ff :: (ClCub Bool) -> SubFace -> State (Set.Set (Address , Address)) (ClCub Bool)    
    ff y sf =        
        
      let caddr' = cAddressSubFace x caddr sf
          cubO = appLI sf (clCub cub)
          f n addr x = 
             if Set.member addr (cAddress caddr')
             then --trace (show (clCub cub) ) $ Right Nothing
                   case unifyOCub (void x) (void cubO) of
                     Left () -> do modify (Set.insert (addr , Address sf []))
                                   return Nothing
                     Right y -> return (Just (True <$ y) )
                  
             else return Nothing
          (newCub , sa) = runState (cubMapMayReplace f y) Set.empty
       in if Set.null sa
          then return newCub
          else do modify (Set.union sa)
                  return (False <$ x)

    fillAllSubFaces' :: State (Set.Set (Address , Address)) ((ClCub Bool))
    fillAllSubFaces' = foldlM ff (False <$ x) (allSubFaces holeDim)


    fillAllSubFaces :: Either FillHoleConflict (ClCub Bool)
    fillAllSubFaces = 
      let (newCub , sa) = runState fillAllSubFaces' Set.empty

      in 
         if Set.null sa
         then Right newCub
         else Left $ FillHoleConflict
                              { fhcOuter = x
                              , fhcHoleCAddress = caddr
                              , fhcCandidate = cub
                              , fhcConflictingAddresses = sa
                              } 
         
              
applyTransform ::  CubTransformation -> ClCub () -> Either CubTransformationError (ClCub Bool)

-- TODO : move to function which cannot fail, becouse this is safe operation
applyTransform (ClearCell caddr OnlyInterior) clcub =
  let tracked = traceConstraints clcub (Set.singleton caddr)
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Right Nothing 
          (Just k , _) -> Right $ Just $ (Cub n (Just k , ()) Nothing) 
  in fmap (fmap (isJust . fst)) $ cubMapMayReplace f tracked

applyTransform (ClearCell caddr WithFreeSubFaces) clcub' =
  let clcub =  void $ fromRight (error "") $ (applyTransform (ClearCell caddr OnlyInterior) clcub')
      tracked = traceConstraints clcub (Set.filter (isFreeAddressClass clcub ) $ cAddrWithSubFaces clcub caddr)
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Right Nothing 
          (Just k , _) -> Right $ Just $ (Cub n (Just k , ()) Nothing) 
  in fmap (fmap (isJust . fst)) $ cubMapMayReplace f tracked

applyTransform (FillHole caddr cub) x = first CubTransformationConflict $ fillHole x caddr cub

applyTransform (SplitCell caddr) clcub =
  let tracked = traceConstraintsSingle clcub caddr
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Right Nothing 
          (Just sf , _) -> Right $ Just $ (Just undefined , undefined) <$ --undefined is intended here, it should not be evaluated
            (splitOCub sf (fromMaybe (error "bad address") (clCubPick addr clcub))) 
  in fmap (fmap (isJust . fst)) $ cubMapMayReplace f tracked


applyTransform (RemoveSide caddr (sf , RSRKeepLeaves)) clcub = do
  let cub = fromJust $ clCubPick (toAddress caddr) clcub  
  cubWithHole <- fmap void $ applyTransform (ClearCell caddr OnlyInterior) clcub
  let newCell = clCubRemoveSideMaximal sf cub 
  applyTransform (FillHole caddr newCell) cubWithHole 

-- should be safe but uses unsafe operation
applyTransform (AddSide caddr sf) clcub = do
  let parCub@(ClCub (FromLI n f)) = fromJust $ clCubPick (toAddress caddr) clcub
  let parOCub = clInterior parCub
  case parOCub of
    Cub _ _ _ -> Left (CubTransformationError "not address of side cell!")
    Hcomp _ mbN (CylCub (FromLI n' g)) a -> do
      cubWithHole <- fmap void $ applyTransform (ClearCell caddr OnlyInterior) clcub
      let f' sff | isFullSF sff =
                    let g' sf' =
                          case (g sf') of
                            Just x -> Just x
                            Nothing -> 
                              if isSubFaceOf sf' sf
                              then Just (Cub (subFaceDimEmb sf' + 1) () Nothing)
                              else Nothing
                    in Hcomp () mbN (CylCub (FromLI n' g')) a
                | otherwise = f sff
          newCell = ClCub (FromLI n f') 
      applyTransform (FillHole caddr newCell) cubWithHole 

  

splitOCub :: SubFace -> ClCub () ->  OCub ()
splitOCub sf x@(ClCub xx) = -- clInterior x
     Hcomp () Nothing (CylCub $ FromLI (getDim x) cylF) x 
  where
    cylF sf' | sf `isSubFaceOf` sf' = Nothing
             | otherwise = --Just (Cub (subFaceDimEmb sf' + 1) () Nothing)
                 -- Just $ oDegenerate ((getDim x) - 1) $ appLI sf' xx
                 Just $ oDegenerate (subFaceDimEmb sf') $ appLI sf' xx


resolveConflict :: FillHoleConflict -> HoleConflictResolutionStrategy -> ClCub ()
resolveConflict fhc ClearOutwards =
  let caddr = fhcHoleCAddress fhc
      caddrList = Set.toList $ Set.map ((addressClass $ fhcOuter fhc) . fst) $ fhcConflictingAddresses fhc
      cleared = foldl
                  (\cu caddr' -> either (error "fatal") void
                      $ applyTransform (ClearCell caddr' OnlyInterior) cu)
                  (fhcOuter fhc) caddrList 
  in either (error "fatal - unification failed after resolving") void $ fillHole cleared caddr (fhcCandidate fhc)

resolveConflict fhc ClearInwards = 
  let caddr = fhcHoleCAddress fhc
      caddrList = Set.toList $ Set.map ((addressClass $ fhcCandidate fhc) . snd) $ fhcConflictingAddresses fhc
      cleared = foldl
                  (\cu caddr' -> either (error "fatal") void
                      $ applyTransform (ClearCell caddr' OnlyInterior) cu)
                  (fhcCandidate fhc) caddrList 
  in either (error "fatal - unification failed after resolving") void $ fillHole (fhcOuter fhc) caddr cleared

     
resolveConflict fhc _ = error "todo"

-- applyTransform (ClearCell addrToReplace OnlyInterior) z =
--    cubMapMayReplace 
--     (\n addr x ->
--        if addr == addrToReplace
--        then --Right $ Just (clInterior valToPut)
--             let bndr = clBoundary $ fromRight (error "imposible") $ clCubPick addr z
                
--             in undefined
--        else Right $ Nothing
--      ) z


-- applyTransform (SplitCell addrToSplit ) =
--    flip cubMapMayReplace [] $ 
--     (\n addr x ->
--        if addr == addrToSplit
--        then  let sides = Map.fromList
--                     (map (\fc -> (toSubFace fc , cubHole n )) $ genAllLI n )
--                     -- needs injectDim to work!
--                     -- (map (\fc@(Face n (i , b)) -> (toSubFace fc , injDim i $ cubFace fc x )) $ genAllLI n )

--              in Just $ Right $ (Hcomp () "splitVar" sides x)
--        else Nothing
--      )

-- applyTransform (RemoveCell [] ) = Right
-- applyTransform (RemoveCell (addrToRemoveHead : addrToRemoveTail) ) =
--    flip cubMapMayReplace [] $ 
--     (\n addr x ->
--        if addr == addrToRemoveTail
--        then case x of
--                Hcomp () nm sides x ->
--                   Just $ Right $ Hcomp () nm (Map.delete addrToRemoveHead sides) x
--                _ -> Nothing
--        else Nothing
--      )

-- applyTransform (RemoveCellLeaveFaces [] ) = Right
-- applyTransform (RemoveCellLeaveFaces (addrToRemoveHead : addrToRemoveTail) ) =
--    flip cubMapMayReplace [] $ 
--     (\n addr parX ->
--        if addr == addrToRemoveTail
--        then case parX of
--                Hcomp () nm sides x ->
--                   Just $ Right $ Hcomp () nm
--                             (deleteButLeaveFaces
--                                -- (const (Cub undefined (Left 0)))
--                                 (\fc -> cubFace (injFaceSide fc) )
--                                 addrToRemoveHead sides) x
--                _ -> Nothing
--        else Nothing
--      )


-- applyTransform (AddSubFace (addrToAdd , sf)) = 
--    flip cubMapMayReplace [] $ 
--     (\n addr x ->
--        if addr == addrToAdd
--        then case x of
--                Hcomp () nm sides x ->
--                   Just $ Right $ Hcomp () nm
--                             (addSubFace
--                                (cubHole n)
--                                sf sides) x
--                _ -> Nothing
--        else Nothing
--      )


-- applyTransform (MapAt addrToMap f) =
--    flip cubMapMayReplace [] $ 
--     (\n addr x ->
--        if addr == addrToMap
--        then Just $ f x
--        else Nothing
--      ) 

