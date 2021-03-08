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

data ClearCellRegime =
    OnlyInterior
  | WithFreeSubFaces
 deriving (Eq)


data ConstraintsOverrideStrategy =
     COSClearWithConstrained
   | COSClearNecessary
   | COSInflate
  deriving (Eq)

data ConstraintsOverrideRegime = NoConstraintsOverride | ConstraintsOverride ConstraintsOverrideStrategy

   
data CubTransformation =
     ClearCell CAddress ClearCellRegime
   | SplitCell CAddress
   | FillHole CAddress (ClCub ())
   -- | ReplaceAt Address (ClCub ())
   -- | RemoveCell Address
   -- | RemoveCellLeaveFaces Address
   -- | AddSubFace (Address , SubFace)
   -- | MapAt Address (ClCub () -> Either String (ClCub ()))
  -- deriving (Show)

-- TODO :: transofrmations for filling - automaticly recognises holes in faces of compositions  which can be removed


data CubTransformationError = CubTransformationError String



instance Show CubTransformationError where
  show (CubTransformationError x) = x


-- rodzaje nadpisywania adresu tylko dla hole :
-- -- tylko hole w inerior
-- -- hole w interior + wolne scinay
-- rodzaje nadpisywania adresu



applyTransform ::  CubTransformation -> ClCub () -> Either CubTransformationError (ClCub Bool)

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

applyTransform (FillHole caddr cub) x =
  fillAllSubFaces x
      
  -- let f n addr x =
  --       if Set.member addr (cAddress caddr)
  --       then Right Nothing
  --       else Right Nothing
  -- in do clcub' <- cubMapMayReplace f clcub
  --       return $ False <$ clcub'
  where
    -- this function should ba aplied always in specyfic , for subaddreses of incrasing dimension 
    fillHoleUnsafe :: SubFace -> OCub () -> ClCub Bool -> Either CubTransformationError (ClCub Bool)
    fillHoleUnsafe = undefined

    holeDim = addresedDim $ head $ Set.toList (cAddress caddr)


    -- TODO : boleans here are set to True, even if no transformation is aplied (becouse term is already there)
    ff :: (ClCub Bool) -> SubFace -> Either CubTransformationError (ClCub Bool)
    ff y sf =
      let caddr' = cAddressSubFace x caddr sf
          cubO = appLI sf (clCub cub)
          f n addr x = 
             if Set.member addr (cAddress caddr')
             then --trace (show (clCub cub) ) $ Right Nothing
                   case unifyOCub (void x) (void cubO) of
                     Left () -> Left (CubTransformationError "unification Fail")
                     Right y -> Right (Just (True <$ y) )
                  
             else Right Nothing
      in cubMapMayReplace f y

    fillAllSubFaces :: ClCub () -> Either CubTransformationError (ClCub Bool)
    fillAllSubFaces x = foldlM ff (False <$ x) (allSubFaces holeDim)

applyTransform (SplitCell caddr) clcub =
  let tracked = traceConstraintsSingle clcub caddr
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Right Nothing 
          (Just sf , _) -> Right $ Just $ (Just undefined , undefined) <$ --undefined is intended here, it should not be evaluated
            (splitOCub sf (fromMaybe (error "bad address") (clCubPick addr clcub))) 
  in fmap (fmap (isJust . fst)) $ cubMapMayReplace f tracked



splitOCub :: SubFace -> ClCub () ->  OCub ()
splitOCub sf x@(ClCub xx) = -- clInterior x
     Hcomp () Nothing (CylCub $ FromLI (getDim x) cylF) x 
  where
    cylF sf' | sf `isSubFaceOf` sf' = Nothing
             | otherwise = --Just (Cub (subFaceDimEmb sf' + 1) () Nothing)
                 Just $ oDegenerate ((getDim x) - 1) $ appLI sf' xx


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

