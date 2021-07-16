{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


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
import Data.Functor.Identity

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

import Abstract

import Debug.Trace

import Control.Monad.State.Strict

import Control.Monad.Writer

import ConsolePrint


ntrace _ x = x

-- use only for subfaces without parents in particular cylinder
-- TODO : good place to intoduce NotFullSubFace type
clCubRemoveSideMaximal :: SubFace -> ClCub () -> ClCub ()
clCubRemoveSideMaximal sf clcub@(ClCub (FromLI n g))
  | isFullSF sf = error "subface of codim > 0 is required here"
  | otherwise = 
  case (clInterior clcub) of
    Cub {} -> error "fatal, clCubRemoveSideMaximal accept only adequate ClCubs, suplied arg is not Hcomp" 
    Hcomp _ mbn si@(CylCub (FromLI n f)) a ->
      if (isNothing (appLI sf (cylCub si)))
      then error "fatal, clCubRemoveSideMaximal accept only adequate ClCubs, suplied arg do not have particular subface"
      else let f' sfCyl | getDim sfCyl /= getDim sf = error "fatal"
                        | sfCyl == sf = Nothing
                        | otherwise = f sfCyl
               g' sfBd | getDim sfBd /= getDim sf = error "fatal"
                       | isFullSF sfBd = Hcomp () mbn (CylCub (FromLI n f')) a
                       | sf == sfBd =
                           let a' = clCubSubFace sf a
                               f'' sfCyl' | isFullSF sfCyl' = Nothing
                                          | otherwise = f $ injSubFace sfCyl' sf                                
                           in --Cub (subFaceDimEmb sf) () Nothing
                              -- if subFaceDimEmb sf == 0
                              -- then clInterior a'
                              -- else
                                Hcomp () mbn (CylCub (FromLI (subFaceDimEmb sf) f'')) a'
                       | sf `isSubFaceOf` sfBd = 
                           let sf' = jniSubFace sf sfBd
                               preSideTake = (clCubSubFace sfBd clcub)
                               postSideTake =
                                   trace ("\n----\n" ++ printClOutOfCtx preSideTake)
                                          clCubRemoveSideMaximal sf'
                                            preSideTake
                               
                               postSideTakeInterior = trace ("\n" ++ printOOutOfCtx (clInterior postSideTake)) clInterior $ postSideTake
                           in postSideTakeInterior
                                
                                 
                       | otherwise = g sfBd
               res = ClCub (FromLI n g')
           in res





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

data SplitCellMode = AlsoSplitParents | SubstWithSplited

                      
data CubTransformation =
     ClearCell CAddress ClearCellRegime
   | SplitCell SplitCellMode CAddress
   | SubstAt CAddress (ClCub ())
   | RemoveSide CAddress (SubFace , RemoveSideRegime)
   | AddSide CAddress SubFace
   -- | ReplaceAt Address (ClCub ())
   -- | RemoveCell Address
   -- | RemoveCellLeaveFaces Address
   -- | AddSubFace (Address , SubFace)
   -- | MapAt Address (ClCub () -> Either String (ClCub ()))
  -- deriving (Show)

-- TODO :: transofrmations for filling - automaticly recognises holes in faces of compositions  which can be removed

ct2CAddress :: CubTransformation -> CAddress
ct2CAddress = \case
     ClearCell x _ -> x
     SplitCell _ x -> x
     SubstAt x _ -> x
     RemoveSide x _ -> x
     AddSide x _ -> x  
  


type SubstAtConflict a1 a2 = ((CAddress , (ClCub a1 ,ClCub a2)),
                  ClCub (PreUnificationResult (a1 , Address) (a2 , Address)))

data SubstAtConflictResolutionStrategy =
    ClearOutwards
  | ClearInwards
  | InflateInwards
  | InflateOutwards
  deriving (Enum, Show, Bounded, Eq)

data CubTransformationError =
     CubTransformationError String
   | CubTransformationConflict (SubstAtConflict () ())

 


instance Show CubTransformationError where
  show (CubTransformationError x) = x
  show (CubTransformationConflict x) = show x




-- substAtTry :: CAddress -> ClCub (SubstAtUnificationResult a1 a2)
--                   -> Either (SubstAtConflict () ()) (ClCub Bool) 
-- substAtTry a x | any isConflictSAUR x = Left (a , fmap (bimap (\_ -> ()) (\_ -> ())) x)
--                | otherwise = Right (fmap isEditedSAUR x)



 -- make it more speclialised, distinction betwen conflict and comaptibility will be good,
 -- compatibility can be additionaly descirbed by Ordering
-- data SubstAtUnificationResultCase a1 a2  =
--      AgreementAtBoundary (ClCub (SubstAtUnificationResult (,) a1 a2))
--    | ConflictAtBoundary (ClCub (SubstAtUnificationResult PreUnificationResult a1 a2))
--    -- | MixedAtBoundary (ClCub (SubstAtUnificationResult PreUnificationResult a1 a2))   
--   deriving (Show)


preSubstAt :: forall a1 a2. ClCub a1 -> CAddress -> ClCub a2 ->
                  ClCub (PreUnificationResult (a1 , Address) (a2 , Address))
preSubstAt x caddr cub = preUnifyClCub x' cub' 
     

  where

    x' :: ClCub (a1 , Address)
    x' = fromJust $ clCubPick (toAddress caddr) $ cubMapWAddr (flip (,)) x

    cub' :: ClCub (a2 , Address)
    cub' = cubMapWAddr (flip (,)) cub

-- preSubstAtCases :: forall a1 a2. 
--                      ClCub (PreUnificationResult (a1 , Address) (a2 , Address))
--                  -> SubstAtUnificationResultCase a1 a2
-- preSubstAtCases x =
--   if all isAgreementSAUR x
--   then AgreementAtBoundary (fromAgreementSAUR <$> x)
--   else ConflictAtBoundary x

      

              
applyTransform ::  CubTransformation -> ClCub () -> Either CubTransformationError (ClCub Bool)

applyTransform (ClearCell caddr OnlyInterior) clcub = Right $ fmap fst $ clearCell caddr clcub


applyTransform (ClearCell caddr WithFreeSubFaces) clcub = Right $ fmap fst $ clearCellWithFreeSF caddr clcub

applyTransform (SubstAt caddr cub) x =
  let p = preSubstAt x caddr cub
      isHoleAtSubstSite = isHole $ clInterior (fromJust $ clCubPick (toAddress caddr) x)
  in if ((all isAgreementQ $ clBoundary p) && (eqDim cub x || isHoleAtSubstSite))
     then Right $ fmap (isJust . snd) $ substInside (clInterior cub) caddr x 
     else Left $ CubTransformationConflict $ ((caddr  , (x , cub )) , p ) 


applyTransform (SplitCell SubstWithSplited caddr) clcub =
  error "todo"

applyTransform (SplitCell AlsoSplitParents caddr) clcub =
  let tracked = traceConstraintsSingle clcub caddr
      f n addr x =
        case (oCubPickTopLevelData x) of
          (Nothing , _) -> Right Nothing 
          (Just sf , _) -> Right $ Just $ (Just undefined , undefined) <$ --undefined is intended here, it should not be evaluated
            (splitOCub sf (fromMaybe (error "bad address") (clCubPick addr clcub))) 
  in fmap (fmap (isJust . fst)) $ cubMapMayReplace f tracked


applyTransform (RemoveSide caddr (sf , RSRRemoveLeavesAlso)) clcub = error "todo"

applyTransform (RemoveSide caddr (sf , RSRKeepLeaves)) clcub = do
  let cub = fromJust $ clCubPick (toAddress caddr) clcub  
  cubWithHole <- fmap void $ applyTransform (ClearCell caddr OnlyInterior) clcub
  let newCell = clCubRemoveSideMaximal sf cub 
  applyTransform (SubstAt caddr newCell) cubWithHole 

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
      applyTransform (SubstAt caddr newCell) cubWithHole 

  

splitOCub :: SubFace -> ClCub () ->  OCub ()
splitOCub sf x@(ClCub xx) = -- clInterior x
     Hcomp () Nothing (CylCub $ FromLI (getDim x) cylF) x 
  where
    cylF sf' | sf `isSubFaceOf` sf' = Nothing
             | otherwise = --Just (Cub (subFaceDimEmb sf' + 1) () Nothing)
                 -- Just $ oDegenerate ((getDim x) - 1) $ appLI sf' xx
                 Just $ oDegenerate (subFaceDimEmb sf') $ appLI sf' xx




-- fhcConflictingAddresses :: SubstAtConflict a1 a2 -> (Set.Set Address , Set.Set Address)

-- fhcConflictingAddresses (_ , cub) = execWriter  $
--   (cubMapTrav f g cub :: Writer (Set.Set Address , Set.Set Address) (ClCub ()))

--   where

--     f _ a b z zz zzz =
--        do h a b
--           return $ ()
--     g k a b z =
--        do h a b
--           return $ ()

--     h :: Address -> (SubstAtUnificationResult PreUnificationResult a1 a2) -> Writer (Set.Set Address , Set.Set Address) ()
--     h addr = \case
--          SAUROutside _ -> return ()
             
--          SAURInside a2 -> return ()
--          SAURBoundary ur -> 
--            case ur of
--              Agreement a1 a2 -> return ()
--              Val1 a1 mba2 -> tell (Set.singleton addr , undefined) 
--              Val2 mba1 a2 -> tell (Set.empty , undefined)

--              Mixed a1 a2 -> return () 
--              Conflict _ _ -> tell (Set.singleton addr , undefined)
      

-- fhcOuter :: SubstAtConflict a1 a2 -> ClCub ()
-- fhcOuter (_ , cub) = void $
--   runIdentity $ cubMapMayReplace f cub

--   where

--     f :: Int -> Address -> OCub (SubstAtUnificationResult PreUnificationResult a1 a2)
--            -> Identity (Maybe (OCub (SubstAtUnificationResult PreUnificationResult a1 a2)))
--     f k _ z =
--       case (oCubPickTopLevelData z) of
--          SAUROutside _ ->
--              Identity $ Nothing
--          SAURInside a2 -> Identity $ Just $ Cub k undefined Nothing
--          SAURBoundary ur -> 
--            case ur of
--              Agreement a1 a2 -> Identity $ Nothing
--              Val1 a1 mba2 -> Identity $ Nothing
--              Val2 mba1 a2 ->
--                Identity $ Just $ Cub k (oCubPickTopLevelData z) Nothing 
--              Mixed a1 a2 -> Identity $ Nothing 
--              Conflict cub' _ -> 
--                Identity $ Just $ fmap undefined cub'

-- fhcOuterForFollowCandidate :: SubstAtConflict a1 a2
--                                 -> (ClCub a1 , [(CAddress , ClCub a2)]) 
-- fhcOuterForFollowCandidate (_ , cub) =
--   undefined
--   -- void $
--   -- runIdentity $ cubMapMayReplace f cub

--   where

--     f :: Int -> Address -> OCub (SubstAtUnificationResult PreUnificationResult a1 a2)
--            -> Identity (Maybe (OCub (SubstAtUnificationResult PreUnificationResult a1 a2)))
--     f k _ z =
--       case (oCubPickTopLevelData z) of
--          SAUROutside _ ->
--              Identity $ Nothing
--          SAURInside a2 -> Identity $ Just $ Cub k undefined Nothing
--          SAURBoundary ur -> 
--            case ur of
--              Agreement a1 a2 -> Identity $ Nothing
--              Val1 a1 mba2 -> Identity $ Nothing
--              Val2 mba1 a2 ->
--                Identity $ Just $ Cub k (oCubPickTopLevelData z) Nothing 
--              Mixed a1 a2 -> Identity $ Nothing 
--              Conflict cub' _ -> 
--                Identity $ Just $ fmap undefined cub'

--     cub' :: ClCub a1
--     cub' = runIdentity $ cubMapMayReplace f cub
           
-- fhcCandidate :: SubstAtConflict a1 a2 -> ClCub ()
-- fhcCandidate  (cAddr , cub') = void $
--   runIdentity $ cubMapMayReplace f cub

--   where

--     cub = fromJust $ clCubPick (toAddress cAddr) cub'
    
--     f :: Int -> Address -> OCub (SubstAtUnificationResult PreUnificationResult a1 a2)
--             -> Identity (Maybe (OCub (SubstAtUnificationResult PreUnificationResult a1 a2)))
--     f k _ z =
--       case (oCubPickTopLevelData z) of
--          SAUROutside _ -> undefined
             
--          SAURInside a2 -> Identity $ Nothing
--          SAURBoundary ur -> 
--            case ur of
--              Agreement a1 a2 -> Identity $ Nothing
--              Val1 a1 mba2 -> Identity $ Just $ Cub k (oCubPickTopLevelData z) Nothing 
--              Val2 mba1 a2 -> Identity $ Nothing

--              Mixed a1 a2 -> Identity $ Nothing 
--              Conflict _ cub' -> 
--                Identity $ Just $ fmap undefined cub'



resolveConflict :: forall a1 a2 . SubstAtConflict a1 a2 -> SubstAtConflictResolutionStrategy -> Maybe (ClCub ())
resolveConflict fhc@((caddr , (outer , inner)) , p) ClearOutwards =
  do toV <- Map.toList <$> toVisit
        
     let outer'' = foldr (\ca -> (fmap snd) . (clearCell ca)) outer' (fst <$> toV) 

     Just $ void $ foldr (\(ca , y) -> (fmap h) . substInside y ca) outer'' toV 

  where

    h :: (Maybe (Maybe a1, Maybe a2), Maybe a2) -> (Maybe a1, Maybe a2)
    h = first (join . fmap fst)

                                                    
    outer' = fmap ((, Nothing) . Just) outer 

    
    fromInnerSubst :: (PreUnificationResult (a1 , Address) (a2 , Address))
                       -> Maybe (CAddress , OCub a2)
    fromInnerSubst = (fmap $ (\x -> (addressClass outer (snd (fst x))
                                         , clInterior $ fromJust $ clCubPick (snd (snd x)) inner ) )) .
      \case
          Val1 a1 (Just a2) -> Just (a1 , a2)
          Val2 (Just a1) a2 -> Just (a1 , a2)      
          Conflict (cub1) (cub2) ->
            Just (oCubPickTopLevelData cub1 , oCubPickTopLevelData cub2)

          Mixed a1 a2 -> Nothing
          Agreement _ _ -> Nothing
          Val1 _ Nothing -> Nothing
          Val2 Nothing _ -> Nothing


    toVisit :: Maybe (Map.Map CAddress (OCub a2)) 
    toVisit =
      foldrM (\(ca , y) z ->
                case Map.lookup ca z of
                  Nothing -> Just (Map.insert ca y z)
                  Just y' -> if (void y') == (void y)
                             then Just z
                             else Nothing
                        
                    )         
                 
        Map.empty (catMaybes $ toList (fromInnerSubst <$> p))


  -- let caddrList = Set.toList $ Set.map ((addressClass $ fhcOuter fhc)) $ fst $ fhcConflictingAddresses fhc
  --     cleared = foldM
  --                 (\cu caddr' -> either (const Nothing) (Just . void)
  --                     $ applyTransform (ClearCell caddr' OnlyInterior) cu)
  --                 (fhcOuter fhc) caddrList
  --     -- filled = foldM
  --     --             (\cu caddr' -> either (const Nothing) (Just . void)
  --     --                 $ applyTransform (ClearCell caddr' OnlyInterior) cu)
  --     --             cleared caddrList
  -- in (\y -> void $ substInsideHoleUnsafe y caddr $ clInterior (fhcCandidate fhc))   <$> cleared
  --     -- either (error "fatal - unification failed after resolving") void
  --     --      $ substAtTry caddr
  --     --      $ substAt cleared caddr (fhcCandidate fhc)

resolveConflict fhc@((caddr , (outer , inner)) , p) ClearInwards =
  if (not $ eqDim outer inner)
  then error " ClearInwards resolution may be use only when dimension of inner term and outer term are equall" -- TODO prevent this to happen by hiding this option in such case
  else do 
        
     toV <- Map.toList <$> toVisit
        
     let inner'' = foldr (\ca -> (fmap snd) . (clearCell ca)) inner' (fst <$> toV) 

         newInner = clInterior $ void $ foldr (\(ca , y) -> (fmap h) . substInside y ca) inner'' toV

     Just $ void $ substInside newInner caddr outer 

  where

    h :: (Maybe (Maybe a0, Maybe a2), Maybe a0) -> (Maybe a0, Maybe a2)
    h (x , y) = (y , join $ fmap snd x) 

                                                    
    inner' = fmap ((Nothing ,) . Just) inner 

    -- todo move outside together with similar one from ClearOutwards
    fromInnerSubst :: (PreUnificationResult (a1 , Address) (a2 , Address))
                       -> Maybe (CAddress , OCub a1)
    fromInnerSubst = 
        (fmap $ (\x -> (addressClass inner (snd (snd x))
                                         , clInterior $ fromJust $ clCubPick (snd (fst x)) outer ) )) .
      \case
          Val1 a1 (Just a2) -> Just (a1 , a2)
          Val2 (Just a1) a2 -> Just (a1 , a2)      
          Conflict (cub1) (cub2) ->
            Just (oCubPickTopLevelData cub1 , oCubPickTopLevelData cub2)

          Mixed a1 a2 -> Nothing
          Agreement _ _ -> Nothing
          Val1 _ Nothing -> Nothing
          Val2 Nothing _ -> Nothing
    
    p' :: BdCub (Maybe (CAddress , OCub a1))
    p' = clBoundary $ fmap fromInnerSubst p  

    toVisit :: Maybe (Map.Map CAddress (OCub a1)) 
    toVisit = 
      foldrM (\(ca , y) z ->
                case Map.lookup ca z of
                  Nothing -> Just (Map.insert ca y z)
                  Just y' -> if (void y') == (void y)
                             then Just z
                             else Nothing
                        
                    )         
                 
        Map.empty (catMaybes $ toList (p'))

resolveConflict fhc@((caddr , (outer , inner)) , p) InflateInwards = error "todo"

resolveConflict fhc@((caddr , (outer , inner)) , p) InflateOutwards = error "todo"

  -- let caddrList = Set.toList $ Set.map ((addressClass $ fhcCandidate fhc) . snd) $ fhcConflictingAddresses fhc
  --     cleared = foldl
  --                 (\cu caddr' -> either (error "fatal") void
  --                     $ applyTransform (ClearCell caddr' OnlyInterior) cu)
  --                 (fhcCandidate fhc) caddrList 
  -- in either (error "fatal - unification failed after resolving") void
  --           $ substAtTry caddr
  --           $ substAt (fhcOuter fhc) caddr cleared

     




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

