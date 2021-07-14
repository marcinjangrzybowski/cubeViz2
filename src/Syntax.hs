{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax where


import Data.List

import qualified Data.Tuple.Extra as DTE

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Data.List.Split
import Data.Maybe
import Data.Bool
import Data.Either
import Data.Function
import qualified Data.List.NonEmpty as Ne

import qualified Text.Read as TR

import DataExtra

import Debug.Trace

import Combi

-- do przemyslenia:
-- moze zrobic werse Conetxtu gdzie nie ma zadncyh dimension?
-- tak zeby nie uzywac Contextu w miejscach gdzie dimension nie maja prawa byc jeszcze zdefiniowane

-- data IExpr =
--    Min IExpr IExpr |
--    Max IExpr IExpr |
--    Dim Int |
--    Neg IExpr | End Bool
--    deriving (Show , Eq , Ord)


-- TODO : parametrise this type by convention of indezes ov variables

data IExpr = IExpr (Set.Set ( Set.Set (Int , Bool)))
  deriving (Eq)

instance Show IExpr where

  show (IExpr x) = show x

remapIExpr :: (Int -> Int) -> IExpr -> IExpr
remapIExpr f (IExpr x) = IExpr (Set.map (Set.map (first f)) x) 

remapIExprDir :: Set.Set Int -> IExpr -> IExpr
remapIExprDir dims (IExpr x) = IExpr (Set.map (Set.map (\(i,b) -> if Set.member i dims then (i , not b) else (i , b) )) x) 



  
setSetElim :: b -> b -> ([[a]] -> b) -> (Set.Set (Set.Set a)) -> b
setSetElim em emSing f s =
  case Set.toList s of
    [] -> em
    x : xs ->
      case Set.toList x of
        [] -> emSing
        _ -> f $ (map Set.toList (x : xs))
        -- if antihereditary property of s is assumed
        -- this branch is guaranted to not contain empty lists

--- this is why IExpr should be implemented via NON EMPTY SETS/MAPS
elimIExpr :: ([[(Int , Bool)]] -> b) -> IExpr -> b
elimIExpr f (IExpr x) =
  (setSetElim (error "wrong IExpr") (error "wrong IExpr") f) x

substIExpr :: (VarIndex -> Maybe IExpr) -> IExpr -> IExpr
substIExpr f x =
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                let a =  case f (VarIndex k) of
                            Nothing -> dim k
                            Just y -> y
                    (IExpr aa) = if b
                               then a
                               else neg a
                 in aa                   
                                    ))) x


  in IExpr (unsafeMaxOf (fmap unsafeMinOf x2)) 

-- slighlty difeent signature
substIExpr' :: [IExpr] -> IExpr -> IExpr
substIExpr' ies x = 
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                let a = ies !! k
                    (IExpr aa) = if b
                                 then a
                                 else neg a
                 in aa ))) x


  in IExpr (unsafeMaxOf (fmap unsafeMinOf x2)) 

  

--this do not refere DimIndexes, but to VarIndexes!
type SubFace2 = Map.Map Int Bool

fcToSubFace2 :: Context -> Face -> SubFace2
fcToSubFace2 ctx (Face _ (i , b)) = Map.fromList [(fromDimI ctx i , b)] 

sfToSubFace2 :: Context -> SubFace -> SubFace2
sfToSubFace2 ctx (SubFace _ sfm) = Map.mapKeys (fromDimI ctx) sfm

sf2ToSubFace :: Context -> SubFace2 -> SubFace
sf2ToSubFace ctx (sfm) = SubFace (getDim ctx) $ Map.mapKeys (toDimI ctx) sfm 



iExprToSubFace2 :: IExpr -> [SubFace2]
iExprToSubFace2 = 
  let z = elimIExpr $ fmap
             (\x ->
               let (eq1 , eq0) = partition snd x
               in if (null $ intersect (map fst eq0) (map fst eq1))
                  then Just (Map.fromList x)
                  else Nothing
               )
  in catMaybes . z 

-- TODO :: This would be good place to return type of subface which cant be Full
iExprToSubFace :: Int -> IExpr -> [SubFace]
iExprToSubFace n = 
  let z = elimIExpr $ fmap
             (\x ->
               let (eq1 , eq0) = partition snd x
               in if (null $ intersect (map fst eq0) (map fst eq1))
                  then Just $ SubFace n (Map.fromList x)
                  else Nothing
               )
  in catMaybes . z 




subFace2ToIExpr :: SubFace2 -> IExpr
subFace2ToIExpr sf2 =
    mapToSet sf2
  & Set.singleton
  & IExpr

subFaceToIExpr :: SubFace -> IExpr
subFaceToIExpr (SubFace _ sf) =
    mapToSet sf
  & Set.singleton
  & IExpr

  
substSubFace2 :: (VarIndex -> Maybe IExpr) -> SubFace2 -> [SubFace2]
substSubFace2 f = (iExprToSubFace2 . substIExpr f . subFace2ToIExpr)

substSubFace :: Int -> [IExpr] -> SubFace -> [SubFace]
substSubFace n ies sf0 = --(iExprToSubFace n . substIExpr' ies . subFaceToIExpr)
  let ie0 = subFaceToIExpr sf0
      ie1 = substIExpr' ies ie0 
      sfs = iExprToSubFace n ie1
      ies' = (fmap snd $ filter (isNothing . fst) $ safeZip (toListLI sf0) ies)
      subs = filter (\sf' ->
                let ies'' = fmap (projIExpr' sf') ies'
                in all isRight ies''
             ) $ nub (concatMap (allSubSubFaces) sfs)
      
  in sfs ++ subs

projIExpr :: SubFace2 -> IExpr -> Either Bool IExpr
projIExpr sf2 x =
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                  case Map.lookup k sf2 of
                    Just bb -> internalEnd (xor bb (not b))  
                    Nothing -> (if b
                                then (dim k)
                                else (neg (dim k))) & (\(IExpr z) -> z)

                                    
                                    ))) x


  in elimUnsafeIExpr (unsafeMaxOf (fmap unsafeMinOf x2))

projIExpr' :: SubFace -> IExpr -> Either Bool IExpr
projIExpr' (SubFace _ sf) x =
  let x2 = elimIExpr (fmap (fmap (
             \(k , b) ->
                  case Map.lookup k sf of
                    Just bb -> internalEnd (xor bb (not b))  
                    Nothing -> (if b
                                then (dim k)
                                else (neg (dim k))) & (\(IExpr z) -> z)

                                    
                                    ))) x

      toPunchOut = Map.keysSet sf
  in elimUnsafeIExpr (unsafeMaxOf (fmap unsafeMinOf x2))
     & second (remapIExpr (fromJust . punchOutMany toPunchOut ))



type FExpr = Set.Set SubFace2

type Face2 = (Int , Bool)

-- codim ?
-- sfDim :: SubFace2 -> Int
-- sfDim = length . Map.keys

faceToFace2 :: Face -> Face2
faceToFace2 (Face n f) = f
  
toFace2 :: SubFace2 -> Maybe Face2
toFace2 sf =
  case (Map.toList sf) of
    [ x ] -> Just x
    _ -> Nothing

face2ToSubFace2 :: Face2 -> SubFace2
face2ToSubFace2 x = Map.fromList [x]

-- minMB :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
-- minMB Nothing _ = Nothing 
-- minMB _ Nothing = Nothing
-- minMB (Just b1) (Just b2) = if b1 == b2 then Just b1 else Nothing



 
unsafeMin e1 e2 =  (makeAntiH $ Set.map (uncurry $ Set.union) $ Set.cartesianProduct e1 e2) 


unsafeMax e1 e2 =
  let y1 = Set.filter (\x -> not (any (flip Set.isSubsetOf x) e2)) e1
      y2 = Set.filter (\x -> not (any (flip Set.isProperSubsetOf x) e1)) y1

  in (Set.union e1 e2)


unsafeMaxOf [] = error "arg of unsafeMaxOf cannot be empty!"
unsafeMaxOf (x : xs) = foldl unsafeMax x xs

unsafeMinOf [] = error "arg of unsafeMinOf cannot be empty!"
unsafeMinOf (x : xs) = foldl unsafeMin x xs

min :: IExpr -> IExpr -> IExpr
min (IExpr e1) (IExpr e2) = IExpr (unsafeMin e1 e2) 

max :: IExpr -> IExpr -> IExpr
max (IExpr e1) (IExpr e2) = IExpr (unsafeMax e1 e2)

dim :: Int -> IExpr
dim x = IExpr (Set.singleton (Set.singleton (x , True)))

internalEnd True = Set.singleton (Set.empty)
internalEnd False = Set.empty


unsafeEnd = error "attempt to create IExpr represetig i0 or i1"
-- end True = Set.singleton (Set.empty)
-- end False = Set.empty


unsafeNeg x = 
  (case Set.maxView x of
    Nothing -> unsafeEnd True
    Just (y , z) ->
      case (Set.maxView y , Set.maxView z) of
        (Nothing , Nothing) -> unsafeEnd False
        (Just _ , Nothing) -> Set.map (Set.singleton . (second not)) y  
        (Nothing , Just _) -> unsafeNeg z
        (Just _ , Just _) ->
           foldr Syntax.unsafeMin ((unsafeNeg . Set.singleton) y) $ Set.map (unsafeNeg . Set.singleton) z)

neg (IExpr x) = IExpr (unsafeNeg x)

elimUnsafeIExpr :: (Set.Set ( Set.Set (Int , Bool))) -> Either Bool IExpr
elimUnsafeIExpr = setSetElim (Left False) (Left True) (Right . IExpr . Set.fromList . fmap (Set.fromList))  


end :: Bool -> IExpr
end = error "attempt to create IExpr represetig i0 or i1"
-- end True = Set.singleton (Set.empty)
-- end False = Set.empty

endView :: IExpr -> Maybe Bool
endView x | x == end True = Just True
          | x == end False = Just False
          | otherwise = Nothing  
          
iFromL :: [[(Int, Bool)]] -> IExpr
iFromL [] = error "attempt to create IExpr represetig i0"
iFromL ([[]]) = error "attempt to create IExpr represetig i1"
iFromL x = IExpr $ (Set.fromList . (map Set.fromList)) x


-- getIExprFace :: Face -> Context -> IExpr -> IExpr
-- getIExprFace (Face _ (i0 , b)) ctx =
--    setSetElim (end False) (end True) (makeAntiH . Set.fromList . (fmap Set.fromList) . f)
  
--   where
--     i :: Int
--     i = fromDimI ctx i0

--     substHelp :: (Int , Bool) -> Maybe (Maybe (Int , Bool))
--     substHelp (j , bb) | j /= i = Just (Just (j , bb))
--                       | bb == b = Nothing -- term evaluates to one, can be removed from /\
--                       | otherwise = Just Nothing -- we note term evaluating to zero, and later discared whole /\
                      
--     f :: [[(Int , Bool)]] -> [[(Int , Bool)]]
--     f l = 
--       let l0 = map (mapMaybe substHelp) l
--           l1 = filter (all isJust) l0
--       in map (map fromJust) l1
    
-- -- mmbi :: (Int , (Maybe Bool)) -> [IExpr]
-- -- mmbi (i , Nothing) = [ Dim i , Neg $ Dim i ]
-- -- mmbi (i , (Just b)) = [(if b then id else Neg) $ Dim i]

-- -- fromN :: IExprN -> IExpr
-- -- fromN = fromL . Set.elems 
-- --   where
-- --     fromL [] = End False
-- --     fromL (x : xs) = foldr (Max . toMin . Map.toList) (toMin . Map.toList $ x) xs
    
-- --     toMin :: [(Int , (Maybe Bool))] -> IExpr
-- --     toMin x = h1 $ concatMap mmbi x 
-- --        where
-- --          h1 :: [IExpr] -> IExpr
-- --          h1 [] = End True
-- --          h1 (y : ys) = foldr Min y ys
    

toSubFace2 :: IExpr -> FExpr
toSubFace2 (IExpr x) =
   (Set.map (Map.fromList . Set.toList . (uncurry Set.union))
   . (Set.filter ((Set.null . (uncurry Set.intersection) . (bimap (Set.map fst) (Set.map fst))) ))
   . (Set.map (Set.partition snd))) x

type Partial = Map.Map SubFace2 Expr

properlyCoveredSubFaces2 :: Context -> Set.Set SubFace2 -> Set.Set SubFace2  
properlyCoveredSubFaces2 ctx =
  (Set.map (sfToSubFace2 ctx) . properlyCoveredSubFaces (getDim ctx) . Set.map (sf2ToSubFace ctx))





--possible performance bootleneck,
-- TODO create costless abstracton from SubFace and SubFace2
partialWithSF :: Context -> Maybe Name -> Partial -> Map.Map SubFace2 Expr
partialWithSF ctx nm pa = 
  let ctx1 = addDimToContext ctx nm

      definedParent :: [(a , Expr)] -> (a , Expr)
      definedParent [x] = x
      definedParent ((_ , Hole _) : xs) = definedParent xs
      definedParent (x : _) = x
      
      mkMissing :: SubFace2 -> Expr
      mkMissing sf2 =
        let sf = (sf2ToSubFace ctx sf2)
            (sfParent , parent) =  
               
                 definedParent
               $ filter ( (isSubFaceOf sf . fst))
               $ (map (first $ sf2ToSubFace ctx) (Map.toList pa))
            ctx2 = addSFConstraintToContext sfParent ctx   
            sf2' = (sfToSubFace2 ctx2 (jniSubFace sf sfParent))
            ctx2' = addSF2ConstraintToContext sf2 ctx1
        in substProj ctx2' sf2' parent
      missing = (Map.fromSet (mkMissing) (properlyCoveredSubFaces2 ctx (Map.keysSet pa)) )
  in Map.union pa missing
       --(trace (show (Map.size missing)) (missing))

ensurePartial :: Map.Map SubFace2 Expr -> Either Expr Partial
ensurePartial m =
  -- trace ("\nfcs" ++ show (fmap (Map.toList . fst) (Map.toList m)) ++ "\n\n") $
  case fmap (first Map.toList) (Map.toList m) of
    (([] , p) : _) -> Left p
    _ -> Right (makeAntiHKeys m)

partialEmpty :: Partial
partialEmpty = Map.empty

partialConst :: IExpr -> Expr -> Partial
partialConst i e = Map.fromSet (\_ -> e) (toSubFace2 i)
  
primPOr :: IExpr -> IExpr -> IExpr -> Partial -> Partial -> Either String Partial
primPOr ei ei1 ei2 pa1 pa2 =
  if (ei == Syntax.max ei1 ei2)
  then Right $ Map.union pa1 pa2
  else Left $ "primPOr err - " ++ "\n\n" ++ show ei
                               ++ "\n\n" ++ show ei1
                               ++ "\n\n" ++ show ei2
                               ++ "\n\n" ++ (show $ Syntax.max ei1 ei2) 

data VarIndex = VarIndex Int
  deriving (Eq , Show, Ord)

data DimIndex = DimIndex Int
  deriving (Eq , Show)


varIndex :: Context -> Int -> Maybe VarIndex
varIndex (Context vs _) i =
  if (i < length vs)
  then (Just (VarIndex i))
  else Nothing

type IArg = ((Expr , Expr) , IExpr)

-- road to working projections :

-- all expressions in CType faces must have same arrity = dimOfCType - 1
  
-- all expressions in CType cannot have free variables


data Expr =
  HComp (Maybe Name) Partial Expr
  | Var VarIndex [IArg]
  | Hole Int
  deriving (Eq , Show)

type LExpr = ([Maybe String] , Expr)  

data CellExpr = CellExpr VarIndex [IExpr]
  deriving (Show , Eq)


iLam :: Maybe String -> LExpr -> LExpr
iLam mbs = first ( (:) mbs )  
-- f should be injective here! (relatvie to expected domain)
remapIExpInExpr :: (Int -> Int) -> Expr -> Expr
remapIExpInExpr  f = \case 
    HComp nm pa e ->
      HComp nm
      (Map.map (remapIExpInExpr  f) (Map.mapKeys (Map.mapKeys f) pa) )
      (remapIExpInExpr f e)
    Var vi tl -> Var vi (fmap (bimap (mapBoth $ remapIExpInExpr f) (remapIExpr f)) tl)
    Hole i -> Hole i

-- here indexes are DIM INDEXES
-- conversion happens in toCub in Abstract.hs
-- TODO : wrap dim indexes into datatype to avoid confusion

-- TODO : document why this is even needed


arityForceRepair :: Context -> Int -> LExpr -> LExpr
arityForceRepair ctx k x@(mbNs , Var vi tl)
      | (arity /= k) = error "incompatibile arity"            
      | (arity < lamArity) = error "imposible"
      | (arity == lamArity) = x
      | (arity > lamArity) =
           let missingN = arity - lamArity
               maxIndex = (depth + k) - 1
               startIndex = maxIndex - missingN + 1
               newTail = (map snd tl) ++ (map dim [startIndex..maxIndex])
               lamDesc = mbNs ++ (replicate missingN $ Nothing) 
               ctxInside = foldl (addDimToContext ) ctx lamDesc  
           in ( lamDesc , mkVar ctxInside vi newTail)

      where
        arity = length (mbNs) + (getCTypeDim (getVarType ctx vi)) - (length tl)

        lamArity = length mbNs

        depth = getDim ctx

      -- | k == (length mbNs) = x
      -- | k < (length mbNs) = error "unable to repair arity"
      -- | k > (length mbNs) =
      --      let missingN = k - (length mbNs)
      --          newTail = (
      --            map snd tl) ++ (map dim [(length mbNs)..(k-1)])
      --      in ( mbNs ++ (replicate missingN $ Nothing) , mkVar ctx vi newTail)
    
arityForceRepair _ _ x = x




multMapKey :: Ord k => (k -> a -> Map.Map k a) -> Map.Map k a  -> Map.Map k a
multMapKey f m = Map.unions $ fmap snd $ Map.toList (Map.mapWithKey f m) 




-- IExpr CANT be Ends (I0 or I1)


substIVars :: Context -> [IExpr] -> Expr -> Expr
substIVars ctx ies = 
    (\case 
    HComp nm pa e ->
       let ctx1 = addDimToContext ctx nm

           mappedSubfaces = Map.fromList $ concat $
             fmap
               (\(sf  , ve) ->
                  let sf2s = substSubFace2 (\(VarIndex i) -> look i ies) sf
                      ctx2 = addSF2ConstraintToContext sf ctx1
                      ve2 = substIVars ctx2 ies ve
                      ves = flip fmap sf2s
                             (\sff -> (sff , substProj ctx2 sff ve2))
                              
                  in ves
               )
               (Map.toList pa)
               

       in case ensurePartial mappedSubfaces of
             Left e -> error "should not be possible here"
             Right pa -> HComp nm pa (substIVars ctx ies e)

       
    Var vi tl ->
      Var vi
        (fmap (bimap (mapBoth $ (substIVars ctx ies)) (substIExpr (\(VarIndex i) -> look i ies))) tl)
    Hole i -> Hole i)


-- result is DimIndexed!
exprFaces :: Context -> Expr -> FromLI Face Expr
exprFaces ctx e =
   let n = getDim ctx
   in FromLI n
        (\fc -> 
           substProj ctx (fcToSubFace2 ctx fc) e
        ) 

-- result is DimIndexed!
exprSubFaces :: Context -> Expr -> FromLI SubFace Expr
exprSubFaces ctx e = 
   let n = getDim ctx
   in FromLI n
        (\sfc ->
           substProj ctx (sfToSubFace2 ctx sfc) e
        ) 

toVarIndexUnsafe :: Expr -> VarIndex
toVarIndexUnsafe (Var vi []) = vi 
toVarIndexUnsafe _ = error "not a term of dimension 0!"

subsetToSubFace2 :: Context -> Subset -> SubFace2
subsetToSubFace2 ctx =
  Map.fromList . (map $ first $ fromDimI ctx) . (zip [0..]) . toListLI 

exprCorners :: Context -> Expr -> FromLI Subset VarIndex
exprCorners ctx e =
  let n = getDim ctx
  in FromLI n
      (toVarIndexUnsafe
      . flip (substProj ctx) e
      . subsetToSubFace2 ctx) 

isHoleExpr (Hole _) = True
isHoleExpr _ = False

subFace2ProjSplit :: SubFace2 -> SubFace2 -> (Maybe (SubFace2 , SubFace2)) 
subFace2ProjSplit sf sf' =
   let z = (all id $ Map.elems $ Map.intersectionWithKey
            (\k -> \b -> \b' -> b' == b )
             sf sf')
   in
      if z then
              let w = Just (Map.difference sf sf' , Map.difference sf' sf)
              in  w
      else Nothing

dropDimInside :: Int -> Expr -> Expr 
dropDimInside k = remapIExpInExpr (fromJust . punchOut k)
   
  
substProj :: Context -> SubFace2 -> Expr -> Expr
substProj ctx sf =
  -- trace ("\n---- "++ (show sf) ++ "---\n" ++ (show (reverse (allDims ctx)) ) ++ "\n" ++ (toStringEE ctx e) ++ "\n-----\n")
  (\case 
    HComp nm pa e ->
       -- TODO : eficiency!
      
       let ctx1 = addDimToContext ctx nm
           mappedSubfaces = Map.fromList $ catMaybes $
             fmap
               (\(sfC  , ve) -> 
                  let sf2s = subFace2ProjSplit sf sfC
                      ctx2 = addSF2ConstraintToContext sfC ctx1
                      ves = flip fmap sf2s
                             (\(sfP,sff) -> (sff , substProj ctx2 sfP ve))
                              
                  in ves
               )
               (Map.toList pa)

       in case ensurePartial mappedSubfaces of
             Left e' ->
                let ctx3 = (addSF2ConstraintToContext sf ctx1)
                    k' = ((length (allDims ctx3)) - 1)
                    e2' = (substProj ctx3 (Map.singleton
                                             k' True)
                                            e')
                in dropDimInside k' e2'
             Right pa -> HComp nm pa (substProj ctx sf e)

      
    Var vi tl ->
      let (endsA , ieA) = partition (isLeft . fst ) $ (zip (map (projIExpr sf . snd) tl) (map fst tl))
          
      in case endsA of
            [] -> Var vi (flip fmap (zip tl ieA) (
                   \((en , _) , (Right ie , _)) -> (mapBoth (substProj ctx sf) en  , ie)
                   ))
            ((Left b , (e0 , e1)) : _)
               -> substProj ctx sf $ if b then e1 else e0
    Hole i -> Hole i)


-- TODO : how exacly those to function are related? descirbe their inputs and results in details

deContextualizeFace :: Context -> Expr -> LExpr
deContextualizeFace ctx e =
   ( unConstrainedDimsOnlyMb ctx , e') 
  where
    e' = 
      remapIExpInExpr
               (fromJust . punchOutMany (Set.fromList $ constrainedDimsOnlyIds ctx) ) e
               
-- TODO :: arity checking!!
contextualizeFace :: Context -> [IExpr] -> LExpr -> Expr
contextualizeFace ctx@(Context _ dims) tl =
    unLiftNotArguments
  . substIVars ctx tl
  . liftNotArguments
  . snd

  where
    n = length tl

    m = length dims
    
    liftNotArguments :: Expr -> Expr
    liftNotArguments =
      remapIExpInExpr
               (\i -> if i < n
                      then i
                      else i + m)
    unLiftNotArguments :: Expr -> Expr
    unLiftNotArguments =
      remapIExpInExpr
               (\i -> if i < n + m
                      then i
                      else i - n)

mkVar :: Context -> VarIndex -> [IExpr] -> Expr
mkVar ctx vi tl =
  let (CType _ ctFcs) = getVarType ctx vi

      tll = zipWith (mapBoth . contextualizeFace ctx) (explode tl) ctFcs

  in Var vi (zip tll tl)

-- this recives tail in VarIndexes
mkCellExpr :: (Env , Context) -> VarIndex -> [IExpr] -> CellExpr
mkCellExpr (ee@(env , ctx)) vI tl = 
  ( CellExpr vI (map ( (remapIExpr (toDimI ctx))) tl) )



-- mkCellExprForce :: (Env , Context) -> VarIndex -> [IArg] -> CellExpr
-- mkCellExprForce (ee@(env , ctx)) vI tl = 
--   let ct = getVarType ctx vI in
--       case (compare (getCTypeDim ct) (length tl)) of
--         EQ -> mkCellExpr ee vI tl
--         LT -> mkCellExpr ee vI (take (getCTypeDim ct) tl)
--         GT -> if (ctxDim ctx == 0)
--               then let firstCornerVI = head $ toListFLI $ (varCorners env ctx vI)
--                    in mkCellExpr ee firstCornerVI []
--               else let tailEnd = range 
--                    in mkCellExpr ee vI (tl ++ 


-- ctx MUST by at last of dimension one here
mkCellExprForce :: (Env , Context) -> VarIndex -> CellExpr
mkCellExprForce ee@(env , ctx) vI
   | getDim ctx == 0 = error "Ctx MUST be at last of dimension 1 here!" 
   | otherwise =
       let tl = fmap (dim . (fromDimI ctx)) (take (getCTypeDim (getVarType ctx vI))
                                               ( (range (getDim ctx)) ++ repeat ((getDim ctx) - 1))  ) 
       in toCellExpr ee (mkVar ctx vI  tl )

-- tail is refering to dimIndexes!
mkCellExprForceDefTail :: (Env , Context) -> VarIndex -> [IExpr] -> CellExpr
mkCellExprForceDefTail ee@(env , ctx) vI dTail
   | getDim ctx == 0 = error "Ctx MUST be at last of dimension 1 here!" 
   | otherwise =
       let dTailLong = fmap (remapIExpr (fromDimI ctx)) $
                          dTail ++ fmap dim  ((range (getDim ctx)) ++ repeat ((getDim ctx) - 1))
           tl =  (take (getCTypeDim (getVarType ctx vI))
                                               (dTailLong)  ) 
       in toCellExpr ee (mkVar ctx vI  tl )
   
toCellExpr :: (Env , Context) -> Expr -> CellExpr
toCellExpr ee (Var vi tl) = mkCellExpr ee vi (map snd tl) 
toCellExpr _ _ = error "fatal, expected Var!"
  
-- fromCellExpr :: (Env , Context) -> CellExpr -> Expr 
-- fromCellExpr (ee@(env , ctx)) (CellExpr vI tl) = 
--   ( Var vI (map (second (remapIExpr (fromDimI ctx))) tl) )

fromCellExprSafe :: (Env , Context) -> CellExpr -> Expr 
fromCellExprSafe (ee@(env , ctx)) (CellExpr vI tl) = 
  mkVar ctx vI (map ( (remapIExpr (fromDimI ctx))) tl)


negateCellExprShallow :: (Set.Set Int ) -> CellExpr -> CellExpr
negateCellExprShallow dims (CellExpr x y) =
  CellExpr x $
    fmap (remapIExprDir dims) y
  
negateCellExpr :: (Set.Set Int ) -> CellExpr -> CellExpr
negateCellExpr dims (CellExpr x y) = undefined

remapCellExprShallow :: (Int -> Int) -> CellExpr -> CellExpr
remapCellExprShallow f (CellExpr x y) =
  CellExpr x $
    fmap (remapIExpr f) y     

degenerateCellExpr :: Int -> CellExpr -> CellExpr
degenerateCellExpr k = remapCellExprShallow (punchIn k) 

remapCellExpr :: (Int -> Int) -> CellExpr -> CellExpr
remapCellExpr f (CellExpr x y) = undefined
  -- CellExpr x $
  --   fmap (second (remapIExpr f)) y     

-- here indexes are DIM INDEXES

data PieceExpr = PieceExpr VarIndex [(Int , Bool)] 
  deriving Show

data BType = BType Int
  deriving (Eq , Show)

data CType =
  CType BType [(LExpr , LExpr)]
  deriving (Eq , Show)


getCTypeDim (CType _ l) = length l

-- TODO : dodac array leveli
-- TODO : dodac array primitive typow
-- TODO : dodac liste equations, w nich kodowac "brzegi" kolejnych typow

type Name = String

--TODO : define as record
data Env = Env [Name] [(Name , Int)]
   deriving Eq

singleLevelAndTypeEnv :: Env
singleLevelAndTypeEnv = Env ["ℓ"] [("A" , 0)]

unsfDefTy :: CType
unsfDefTy = CType (BType 0) []





-- TODO : describe indexing conventions in Context and Env !! 
data Context = Context [(String,CType)] [(Maybe String , Maybe Bool)] -- (Maybe CType)
                deriving Show

addLevelToEnv :: Env -> Name -> Env
addLevelToEnv (Env ls bts) name = Env (name : ls) bts

addBTyToEnv :: Env -> Name -> Int -> Env
addBTyToEnv (Env ls bts) name (bTyId) = Env ls ((name , bTyId) : bts) 
  
addVarToContext :: Context -> CType -> String -> Context
addVarToContext (Context t d) tm name = Context ((name , tm) : t) d

addVarToContext' :: Context -> CType -> String -> (Context , Int)
addVarToContext' x@(Context t d) tm name =
   (addVarToContext x tm name , length t)

addDimToContext :: Context -> Maybe String -> Context
addDimToContext (Context t d) name = Context t ((name , Nothing) : d)


mapAtMany :: [(Int , a -> a )] -> [a] -> [a]
mapAtMany [] x = x
mapAtMany _ [] = []
mapAtMany ((0 , y) : ys) (x : xs) = y x : (mapAtMany (map (first (flip (-) 1)) ys) xs)
mapAtMany ys (x : xs) = x : (mapAtMany (map (first (flip (-) 1)) ys) xs)


addSF2ConstraintToContext :: SubFace2 -> Context ->  Context
addSF2ConstraintToContext sf (Context t d) =
   Context t
    $ reverse $ mapAtMany (map (second (second . const . Just )) (Map.toList sf)) $ reverse d

addSFConstraintToContext :: SubFace -> Context ->  Context
addSFConstraintToContext sf ct@(Context t d) =
   Context t
    $ reverse $ mapAtMany (map (second (second . const . Just )) (Map.toList (sfToSubFace2 ct sf))) $ reverse d


addFaceConstraintToContext :: Face -> Context ->  Context
addFaceConstraintToContext (Face _ (i , b)) ctx = addSF2ConstraintToContext (Map.fromList [(fromDimI ctx i , b)]) ctx

lookupLevel :: Env -> String -> Maybe Int
lookupLevel (Env ls _) x = ((length ls - 1)-) <$> elemIndex x (ls)

lookupBType :: Env -> String -> Maybe BType
lookupBType (Env  _ dts) x = (BType . ((length dts - 1)-)) <$> elemIndex x (map fst dts) 

allDims :: Context -> [(String , Maybe Bool)]
allDims (Context _ l) = map (first (fromMaybe "_")) $ l 


unConstrainedDimsOnly :: Context -> [String]
unConstrainedDimsOnly (Context _ l) = map ((fromMaybe "_") . fst) $ filter (isNothing . snd) $ l 

unConstrainedDimsOnlyMb :: Context -> [Maybe String]
unConstrainedDimsOnlyMb (Context _ l) = map (fst) $ filter (isNothing . snd) $ l 


unConstrainedDimsOnlyIds :: Context -> [Int]
unConstrainedDimsOnlyIds (Context _ l) = map snd $ filter (isNothing . snd . fst) $ zip (reverse l) [0..] 

constrainedDimsOnlyIds :: Context -> [Int]
constrainedDimsOnlyIds (Context _ l) = map snd $ filter (isJust . snd . fst) $ zip (reverse l) [0..] 


(!!<) :: [a] -> Int -> Maybe a 
l !!< i = look (length l - 1 - i) l



toDimI :: Context -> Int -> Int
toDimI c@(Context _ l) i =
  let ii = i - length (filter isJust (take i (map snd (reverse l))))

  in 
     case (l !!< i) of
       Just (s , Just _) ->
                        -- let z = " \n \n constrainedd dim: " ++ (show i) ++ " " ++ fromMaybe "_" s
                        --              ++ "\n\nctx:" ++ (show l)
                        -- in
                          error $ "constrainedd dim: " ++ (show i) ++ " " ++ fromMaybe "_" s   
       Just (_ , Nothing) -> ii

       Nothing -> error $ "\n\nbad VarI: " ++ show i ++ " ctx :" ++ show l
                             
countTrueBeforeNFalse :: [Bool] -> Int -> Int 
countTrueBeforeNFalse = h
  where
    h [] _ = 0
    h (True : xs) k = 1 + h xs k
    h (False : xs) 0 = 0
    h (False : xs) k = h xs (k - 1)
    

fromDimI :: Context -> Int -> Int
fromDimI c@(Context _ l) i =
     if (i >= ctxDim c || i < 0)
     then error "fromDimI fatal"
     else i + countTrueBeforeNFalse (map  (isJust . snd) (reverse l)) i

-- lookupDim :: Context -> String -> Maybe Int
-- lookupDim c x =
--    let l = unConstrainedDimsOnly c
--    in ((length l - 1)-) <$> elemIndex x l

lookupDim :: Context -> String -> Maybe Int
lookupDim (Context _ l0) x =
   let l = map fst l0 
   in ((length l - 1)-) <$> elemIndex (Just x) l



indexS :: Int -> [ a ] -> Maybe a
indexS k l = if (k < length l) then Just (l !! k) else Nothing

getDimSymbol :: Context -> Int -> Either String (Maybe String)
getDimSymbol (Context _ l) i =
   let j = ((length l - 1)- i)
   in
     if j < 0 then (Left $ "bad dim abstraction! : \n" ++ (show l) ++ "  " ++ show i) 
     else maybe (Left "bad dim abstraction!")
                  (Right . fst)
                  (indexS j l)

lookupVar :: Context -> String -> Maybe Int
lookupVar (Context l _) x = ((length l - 1)-) <$> elemIndex x (map fst l) 

getVarSymbol :: Context -> Int -> Either String String
getVarSymbol (Context l _) i =
  maybe (Left $ "bad var abstraction! : " ++ (show i))
        (Right . fst) (indexS ((length l - 1) - i) l)

getBTypeSymbol :: Env -> BType -> Either String String
getBTypeSymbol (Env _ l) (BType i) =
  maybe (Left $ "bad BType abstraction! : " ++ (show i))
        (Right . fst) (indexS ((length l - 1) - i) l)

getVarType :: Context -> VarIndex -> CType
getVarType (Context l _) (VarIndex i) =
  maybe (error "fatal : varIndex not consistent with context!!")
        id
        (snd <$> (indexS ((length l - 1) - i) l))

varCorners :: Env -> Context -> VarIndex -> FromLI Subset VarIndex
varCorners ee ctx0 vi =
        let ct = getVarType ctx0 vi
            n = getCTyDim ee ctx0 ct
            ctx = foldl addDimToContext ctx0 (replicate n Nothing )
        in exprCorners ctx (mkVar ctx vi (fmap dim $ range n))

printCTypeCorners :: Env -> Context -> CType -> Int -> String 
printCTypeCorners ee ctx0 ct i =
      let n = getCTyDim ee ctx0 ct
          ctx = foldl addDimToContext ctx0 (replicate n Nothing )
          crnrs = exprCorners ctx (mkVar ctx (VarIndex i) (fmap dim $ range n))
      in indent 6 $ ("\n" ++ (intercalate "\n" $ fmap (show) $ toListFLI crnrs))
        
instance Codelike Context where
   toCode (ee , _) (Context ty dims) = Right $
     "CTypes:\n" ++ (intercalate "\n" (
                        -- map (\(nm, val) -> nm ++ " : " ++ show val) ty
                        map 
                        (\((nm , val),k) ->
                             nm ++ " : " ++ (toString (ee , Context (drop k ty) []) val)
                             ++ "\n"
                             ++ (printCTypeCorners ee (Context ty []) val (length ty - k))
                             ++ "\n"
                        )
                          (zip ty [1..]) 
                        
                        ))
     ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> (fromMaybe "_" nm) ++ " : " ++ "X") dims))
  
-- instance Show Context where
--    show (Context ty dims) =
--      "CTypes:\n" ++ (intercalate "\n" (
--                         -- map (\(nm, val) -> nm ++ " : " ++ show val) ty
--                         fst (foldr
--                         (\(nm , val) -> \(l , ctx) ->
--                              (((nm ++ " : " ++ (either id id (toCode ctx val))) : l  ) , ctx))
--                         ([] , (emptyCtx))  ty)
--                         )
--                     ) 
--      ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))

instance Show Env where
   show (Env lvls bTypes) =
     "\nBTypes:\n" ++ (intercalate "\n" (map (\(nm, val) -> nm ++ " : " ++ show val) bTypes)) 
     -- ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> nm ++ " : " ++ "X") dims))


type Boundary = [((Int,Bool),Expr)]


indent :: Int -> String -> String
indent i s = intercalate ("\n" ++ (replicate i ' ')) (splitOn "\n" s) 

class Codelike a where
  toCode :: (Env , Context) -> a -> Either String String

  toStringEE :: Context -> a -> String
  toStringEE ctx = toString (Env [] [] , ctx) 

  toString :: (Env , Context) -> a -> String
  toString c = either id id . toCode c
  
instance Codelike (Int , Bool) where
  toCode (_ , ctx) (i , b) =
    do s <- (getDimSymbol ctx i)
       return ((if b then "" else "~ ")  ++ fromMaybe ("dim" ++ show i) s)
  
instance Codelike IExpr where
  toCode eee x =  
     do l <- traverse (traverse (toCode eee)) (elimIExpr id x)
        return (intercalate " ∨ " (map (intercalate " ∧ ") l))

parr :: String -> String
parr x = "(" ++ x ++ ")" 

instance Codelike Partial where
  toCode c pa =
     do l <- traverse (\(f , e) -> do bo <- toCode c e
                                      fc <- toCode c f
                                      return ( (parr $ parr fc ++ " = i1") ++ " → " ++ bo ++ "\n")

                       ) (Map.toList pa)
        return (intercalate ";" l)


instance Codelike Expr where
  toCode (ee , c) (HComp v pa e) =
     do x <- (toCode (ee , (addDimToContext c v)) pa)
        y <- (toCode (ee , c) e)
        return ("hcomp " ++ "(λ " ++ fromMaybe "todo" v ++ " → λ { " ++ (indent 5 ("\n" ++ x)) ++ "})\n" ++ parr y )
  -- toCode (ee , c) (ILam s e) =
  --    do x <- (toCode (ee , (addDimToContext c s)) e)
  --       return ("λ " ++ fromMaybe "_" s ++ " → " ++ (indent 5 ("\n" ++ x)) ++ "\n" )
  toCode (e , c) (Var (VarIndex h) t) =
     do let l = map
                (\((e0, e1), a) ->
                   -- parr((toString (e , c) e0) ++ "|" ++ (toString (e , c) a)  ++"|"++ (toString (e , c) e1))
                    (toString (e , c) a)
                   )
                   
                      t
        hc <- getVarSymbol c h
        return (hc ++ " " ++ (intercalate " " (map parr l)))
        
  toCode _ (Hole hI) =
     return ("{!!}")

instance Codelike LExpr where
  toCode ee ([] , e) = toCode ee e

  toCode (ee , c) (s : ss , e) =
          do x <- (toCode (ee , (addDimToContext c s)) ( ss , e) )
             return ("λ " ++ fromMaybe "_" s ++ " → " ++ (indent 5 ("\n" ++ x)) ++ "\n" )


instance Codelike CType where
  -- toCode _ = pure . show
  toCode (ee , _) (CType bt []) = getBTypeSymbol ee bt
  toCode (ee , ctx) ct@(CType bt ([(e0 , e1) ])) = 
      do s0 <- toCode (ee , ctx) e0
         s1 <- toCode (ee , ctx) e1
         return (s0 ++ " ≡ " ++ s1)
      
  -- toCode (ee , ctx) ct@(CType bt (x : xs)) = pure $ show ct
    --pure "todo in Syntax: instance Codelike CType" 
  toCode (ee , ctx) ct@(CType bt fcs@(x : xs)) = Right $
     "CType \n" ++
       indent 5 ("\n" ++ (intercalate "\n"
                ((zipWith (\k -> \(e0 , e1) ->
                                (show k) ++ "=0" ++ "\n" ++ e0 ++ "\n\n"
                             ++ (show k) ++ "=1" ++ "\n" ++ e1) [0..])
                    $ map (mapBoth (toString (ee , ctx))) fcs)))   
    
instance Codelike SubFace2 where
  toCode ce f =
    do l <- traverse (toCode ce) (Map.toList f)
       return (intercalate " ∧ " l) 
       

  
-- ppIExpr :: IExpr -> String
-- ppIExpr =
--      (intercalate " ∨ ")
--    . map ( (\x -> "(" ++ x ++ ")") . intercalate " ∧ " . (map (\(i , b) -> if b then show i else "~" ++ (show i))) . Set.toList)
--    . (Set.toList)

-- (intercalate "AND" . (map ()))
  
-- instance Show Expr where
--    show (HComp p e) = "xx"
--    show (Var i t) = show i ++ " " ++ intercalate " " (map ((\x -> "(" ++ x ++ ")") . ppIExpr) t)



------ safe functions

emptyEnv :: Env
emptyEnv = Env [] []

emptyCtx :: Context
emptyCtx = Context [] []

freshCtx :: Int -> Context
freshCtx n = foldl addDimToContext emptyCtx (map Just $ genericDims n)

mkBType :: Env -> Int -> Either String BType
mkBType (Env lN _) bI =
  if (bI < length lN)
  then Right (BType bI)
  else Left "bad level arg"


getBaseType :: Env -> Context -> CType -> BType
getBaseType _ _ (CType bTy _) = bTy


getCTyDim :: Env -> Context -> CType -> Int
getCTyDim e c (CType i faces) = length faces



-- this tells us arity of expresion without looking into dimensions defined in context
getExprDim :: Env -> Context -> Expr -> Int
getExprDim e c (Var vI tl) = 
    ((\k -> k - (length tl)) . getCTyDim e c) (getVarType c vI)
getExprDim e c (HComp _ _ x) = getExprDim e c x
-- getExprDim e c (ILam _ x) = 1 + (getExprDim e c x)
getExprDim e c (Hole _) = 0


-- TODO :: add arity checking
mkCType :: Env -> Context -> (BType , [(LExpr , LExpr)]) -> Either String CType
mkCType e c (bTy , []) = Right (CType bTy []) 
mkCType e c (bTy , faces) =
  let ged = getExprDim e c in
  -- if ([(length faces - 1 , length faces - 1) ] == nub (map (bimap ged ged ) $ faces ))
   (Right (CType bTy faces))
  -- else (Left "faces of wrong dimension")



mkHcomp ::  Env -> Context -> (Maybe String , Partial , Expr) -> Either String Expr
mkHcomp env c (s , pa , e) =
  if (getExprDim env c e == 0)
  then Right (HComp s pa e)
  else Left "only 0-dimensional terms are allowed in center of hcomp"

ctxDim = length . unConstrainedDimsOnly


instance OfDim ((Env , Context) , Expr) where
  getDim ((_ , c) , _) = ctxDim c


instance OfDim Context where
  getDim c = ctxDim c

instance OfDim (Env , Context) where
  getDim (_ , c) = ctxDim c


-- 
-- fExprAllFaces :: Context -> FExpr
-- fExprAllFaces ctx =
--   Set.fromList [ Map.fromList (zip vars sides)
--                | vars <- explode $ unConstrainedDimsOnlyIds ctx 
--                , sides <- map toListLI $ (genAllLI ((getDim ctx) - 1) :: [Subset])
--                ]

fExprAllFaces :: Context -> FExpr
fExprAllFaces ctx =
  Set.fromList [ Map.fromList ([(vars , side)])
               | vars <- unConstrainedDimsOnlyIds ctx
               , side <- [True , False]
               ]

toSSnum :: Char -> Char
toSSnum x =
  case TR.readMaybe [x] of
    Just d -> "₀₁₂₃₄₅₆₇₈₉" !! d
    Nothing -> x
      
--"₀₁₂₃₄₅₆₇₈₉"
  
genericDims n = (["i","j","k"] ++ [ "i" ++ map toSSnum (show i) | i <- range (n - 3) ])
  
mkExprGrid :: Int -> Int ->  ((Env , Context) , Expr)
mkExprGrid n k = ((env , ctxTop) , meg ctxTop k) 

  where
    (env , ctxTop) =
      -- let dims = fmap (flip (,) Nothing) $ take n (["i","j","k"] ++ [ "i" ++ (show i) | i <- range (n - 3) ])
        (Env [] [] , Context [] (replicate n (Nothing,Nothing)) )
  
      
    meg ctx 0 = Hole 0
    meg ctx k =
      let newVar = ( "z" ++ (show (k - 1)))
          newCtx = addDimToContext ctx (Just newVar)
      in  either ( \e -> error $ e ++ "imposible: " ++ (either id id $ toCode (env , ctxTop) ctx) )
                 id (mkHcomp env ctx (Just newVar
                       , Map.fromSet (\sf2 ->
                                        meg (addSF2ConstraintToContext sf2 newCtx) (k - 1)
                                     ) (fExprAllFaces ctx)

                       , meg ctx (k - 1) ))
      
