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

import SyntaxIExpr

type Partial = Map.Map SubFace2 OExpr

-- type ClExpr = SubFace2 -> Expr


properlyCoveredSubFaces2 :: Context -> Set.Set SubFace2 -> Set.Set SubFace2  
properlyCoveredSubFaces2 ctx =
  (Set.map (sfToSubFace2 ctx) . properlyCoveredSubFaces (getDim ctx) . Set.map (sf2ToSubFace ctx))





-- --possible performance bootleneck,
-- -- TODO create costless abstracton from SubFace and SubFace2
-- partialWithSF :: Context -> Maybe Name -> Partial -> Map.Map SubFace2 Expr
-- partialWithSF ctx nm pa = 
--   let ctx1 = addDimToContext ctx nm

--       definedParent :: [(a , Expr)] -> (a , Expr)
--       definedParent [x] = x
--       definedParent ((_ , Hole _) : xs) = definedParent xs
--       definedParent (x : _) = x
      
--       mkMissing :: SubFace2 -> Expr
--       mkMissing sf2 =
--         let sf = (sf2ToSubFace ctx sf2)
--             (sfParent , parent) =  
               
--                  definedParent
--                $ filter ( (isSubFaceOf sf . fst))
--                $ (map (first $ sf2ToSubFace ctx) (Map.toList pa))
--             ctx2 = addSFConstraintToContext sfParent ctx   
--             sf2' = (sfToSubFace2 ctx2 (jniSubFace sf sfParent))
--             ctx2' = addSF2ConstraintToContext sf2 ctx1
--         in substProj ctx2' sf2' parent
--       missing = (Map.fromSet (mkMissing) (properlyCoveredSubFaces2 ctx (Map.keysSet pa)) )
--   in Map.union pa missing
--        --(trace (show (Map.size missing)) (missing))

-- ensurePartial :: Map.Map SubFace2 Expr -> Either Expr Partial
-- ensurePartial m =
--   -- trace ("\nfcs" ++ show (fmap (Map.toList . fst) (Map.toList m)) ++ "\n\n") $
--   case fmap (first Map.toList) (Map.toList m) of
--     (([] , p) : _) -> Left p
--     _ -> Right (makeAntiHKeys m)

partialEmpty :: Partial
partialEmpty = Map.empty

partialConst :: IExpr -> OExpr -> Partial
partialConst i e = Map.fromSet (\_ -> e) (toSubFace2 i)
  
primPOr :: IExpr -> IExpr -> IExpr -> Partial -> Partial -> Either String Partial
primPOr ei ei1 ei2 pa1 pa2 =
  if (ei == SyntaxIExpr.max ei1 ei2)
  then Right $ Map.union pa1 pa2
  else Left $ "primPOr err - " ++ "\n\n" ++ show ei
                               ++ "\n\n" ++ show ei1
                               ++ "\n\n" ++ show ei2
                               ++ "\n\n" ++ (show $ SyntaxIExpr.max ei1 ei2) 


varIndex :: Context -> Int -> Maybe VarIndex
varIndex (Context vs _) i =
  if (i < length vs)
  then (Just (VarIndex i))
  else Nothing

type IArg = ((Expr , Expr) , IExpr)

-- road to working projections :

-- all expressions in CType faces must have same arrity = dimOfCType - 1
  
-- all expressions in CType cannot have free variables


data ConstExpr = ConstExpr 
  deriving (Eq , Show)


data OExpr =
    HComp (Maybe Name) Partial Expr
  | Cell CellExpr
  | ConstE ConstExpr
  | Hole Int
  | Appl Expr [Expr]  
  deriving (Eq , Show)


toOExpr :: OExpr -> Maybe ConstExpr
toOExpr (ConstE x) = Just x
toOExpr _ = Nothing

data Expr = Expr Int (SubFace2 -> OExpr)
 -- deriving (Eq , Show)
-- data Expr =

instance Show Expr where
  show = undefined

instance Eq Expr where
  _==_ = undefined

type LExpr = ([Maybe String] , Expr)  

data CellExpr =
    CellExpr VarIndex [IExpr]

  deriving (Show , Eq)


-- here indexes are DIM INDEXES
-- conversion happens in toCub in Abstract.hs
-- TODO : wrap dim indexes into datatype to avoid confusion

-- TODO : document why this is even needed




multMapKey :: Ord k => (k -> a -> Map.Map k a) -> Map.Map k a  -> Map.Map k a
multMapKey f m = Map.unions $ fmap snd $ Map.toList (Map.mapWithKey f m) 




-- IExpr CANT be Ends (I0 or I1)



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
toVarIndexUnsafe = undefined
-- toVarIndexUnsafe (Var vi []) = vi 
-- toVarIndexUnsafe _ = error "not a term of dimension 0!"

subsetToSubFace2 :: Context -> Subset -> SubFace2
subsetToSubFace2 ctx =
  Map.fromList . (map $ first $ fromDimI ctx) . (zip [0..]) . toListLI 

exprCorners :: Context -> Expr -> FromLI Subset ConstExpr
exprCorners ctx (Expr n f) =
  FromLI n (fromJust (error "not a ConstExpr in Corner!!") . toOExpr . f . subsetToSubFace2 ctx)
      
isHoleExpr :: Expr -> Bool
isHoleExpr = undefined
-- isHoleExpr (Hole _) = True
-- isHoleExpr _ = False

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

   
  
substProj :: Context -> SubFace2 -> Expr -> Expr
substProj ctx sf (Expr n f) = undefined
 -- Expr 
  
  -- -- trace ("\n---- "++ (show sf) ++ "---\n" ++ (show (reverse (allDims ctx)) ) ++ "\n" ++ (toStringEE ctx e) ++ "\n-----\n")
  -- (\case 
  --   HComp nm pa e ->
  --      -- TODO : eficiency!
      
  --      let ctx1 = addDimToContext ctx nm
  --          mappedSubfaces = Map.fromList $ catMaybes $
  --            fmap
  --              (\(sfC  , ve) -> 
  --                 let sf2s = subFace2ProjSplit sf sfC
  --                     ctx2 = addSF2ConstraintToContext sfC ctx1
  --                     ves = flip fmap sf2s
  --                            (\(sfP,sff) -> (sff , substProj ctx2 sfP ve))
                              
  --                 in ves
  --              )
  --              (Map.toList pa)

  --      in case ensurePartial mappedSubfaces of
  --            Left e' ->
  --               let ctx3 = (addSF2ConstraintToContext sf ctx1)
  --                   k' = ((length (allDims ctx3)) - 1)
  --                   e2' = (substProj ctx3 (Map.singleton
  --                                            k' True)
  --                                           e')
  --               in dropDimInside k' e2'
  --            Right pa -> HComp nm pa (substProj ctx sf e)

      
  --   Var vi tl ->
  --     let (endsA , ieA) = partition (isLeft . fst ) $ (zip (map (projIExpr sf . snd) tl) (map fst tl))
          
  --     in case endsA of
  --           [] -> Var vi (flip fmap (zip tl ieA) (
  --                  \((en , _) , (Right ie , _)) -> (mapBoth (substProj ctx sf) en  , ie)
  --                  ))
  --           ((Left b , (e0 , e1)) : _)
  --              -> substProj ctx sf $ if b then e1 else e0
  --   Hole i -> Hole i
  -- )


-- TODO : how exacly those to function are related? descirbe their inputs and results in details


fcToSubFace2 :: Context -> Face -> SubFace2
fcToSubFace2 ctx (Face _ (i , b)) = Map.fromList [(fromDimI ctx i , b)] 

sfToSubFace2 :: Context -> SubFace -> SubFace2
sfToSubFace2 ctx (SubFace _ sfm) = Map.mapKeys (fromDimI ctx) sfm

sf2ToSubFace :: Context -> SubFace2 -> SubFace
sf2ToSubFace ctx (sfm) = SubFace (getDim ctx) $ Map.mapKeys (toDimI ctx) sfm 

              
-- -- this recives tail in VarIndexes
-- mkCellExpr :: (Env , Context) -> VarIndex -> [IExpr] -> CellExpr
-- mkCellExpr (ee@(env , ctx)) vI tl = 
--   ( CellExpr vI (map ( (remapIExpr (toDimI ctx))) tl) )


-- remapCellExprShallow :: (Int -> Int) -> CellExpr -> CellExpr
-- remapCellExprShallow f (CellExpr x y) =
--   CellExpr x $
--     fmap (remapIExpr f) y     

-- degenerateCellExpr :: Int -> CellExpr -> CellExpr
-- degenerateCellExpr k = remapCellExprShallow (punchIn k) 

-- here indexes are DIM INDEXES

data PieceExpr = PieceExpr VarIndex [(Int , Bool)] 
  deriving Show

-- data BType = BType Int
--   deriving (Eq , Show)

data CType = CType Expr

  deriving (Eq , Show)


-- getCTypeDim (CType _ l) = length l

-- TODO : dodac array leveli
-- TODO : dodac array primitive typow
-- TODO : dodac liste equations, w nich kodowac "brzegi" kolejnych typow

type Name = String

--TODO : define as record
data Env = Env --[Name] [(Name , Int)]
   deriving Eq

singleLevelAndTypeEnv :: Env
singleLevelAndTypeEnv = Env

-- unsfDefTy :: CType
-- unsfDefTy = CType (BType 0) []





-- TODO : describe indexing conventions in Context and Env !! 
data Context = Context [(String,CType)] [(Maybe String , Maybe Bool)] -- (Maybe CType)
                deriving Show

-- addLevelToEnv :: Env -> Name -> Env
-- addLevelToEnv (Env ls bts) name = Env (name : ls) bts

-- addBTyToEnv :: Env -> Name -> Int -> Env
-- addBTyToEnv (Env ls bts) name (bTyId) = Env ls ((name , bTyId) : bts) 
  
addVarToContext :: Context -> CType -> String -> Context
addVarToContext (Context t d) tm name = Context ((name , tm) : t) d

addVarToContext' :: Context -> CType -> String -> (Context , Int)
addVarToContext' x@(Context t d) tm name =
   (addVarToContext x tm name , length t)


dimSpecyficGenericVarName :: Int -> Char
dimSpecyficGenericVarName =
  \case
     0 -> 'x'
     1 -> 'p'
     2 -> 's'
     3 -> 'c'
     _ -> error "todo"
  

boundedSymbolsInCtx :: Context -> [String]
boundedSymbolsInCtx (Context t d) = nub ((fst <$> t) ++ (catMaybes (fst <$> d) ))



genFreshName :: [String] -> Char -> String 
genFreshName xs c = c : (toSSnum <$> (show k) )

  where
    
    k =
       let l = zip [0..]
               $ sort
               $ catMaybes
               $ map
                (\case 
                    x : xs ->
                      if x /= c then Nothing
                      else (((traverse fromSSnum xs) >>= TR.readMaybe ) :: Maybe Int) 
                    _ -> error "empty name!") $
                xs
       in maybe (length l) fst
        $ find (uncurry (/=) )
        $ l

genFreshVarName :: Context -> Int -> String
genFreshVarName ctx n =
    genFreshName (boundedSymbolsInCtx ctx) (dimSpecyficGenericVarName n)


genFreshDimName :: Context -> String
genFreshDimName ctx =
    genFreshName (boundedSymbolsInCtx ctx) 'i'

    
    
    

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

-- lookupLevel :: Env -> String -> Maybe Int
-- lookupLevel (Env ls _) x = ((length ls - 1)-) <$> elemIndex x (ls)

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


fillMaybesIndexed :: (Int -> a) -> [Maybe a] -> [a]
fillMaybesIndexed f = fmi 0
  where
    fmi k (Just a : xs) = a : fmi k xs
    fmi k (_ : xs) = f k : fmi (k + 1) xs
    fmi k [] = []

withPlaceholdersDimSymbols :: Context -> [String]
withPlaceholdersDimSymbols (Context _ l) = reverse $ h $ wp
  where

    wp =   fillMaybesIndexed (("𝒊" ++) . toSSnumStr) 
         $ map (fst)
         $ reverse l
    
    f x = map (\y -> if x == y then y ++ "'" else y)       
    
    h [] = []
    h (x : xs) = x : h (f x xs)

getDimSymbolAlwaysPrim :: Context -> Int -> Either String String
getDimSymbolAlwaysPrim ctx@(Context _ l) i = 
   let j = ((length l - 1)- i)
   in
     if j < 0 then (Left $ "bad dim abstraction! : \n" ++ (show l) ++ "  " ++ show i) 
     else maybe (Left "bad dim abstraction!")
                  (Right)
                  (indexS j (withPlaceholdersDimSymbols ctx))


lookupVar :: Context -> String -> Maybe Int
lookupVar (Context l _) x = ((length l - 1)-) <$> elemIndex x (map fst l) 

getVarSymbol :: Context -> Int -> Either String String
getVarSymbol (Context l _) i =
  maybe (Left $ "bad var abstraction! : " ++ (show i))
        (Right . fst) (indexS ((length l - 1) - i) l)


getVarType :: Context -> VarIndex -> CType
getVarType (Context l _) (VarIndex i) =
  maybe (error "fatal : varIndex not consistent with context!!")
        id
        (snd <$> (indexS ((length l - 1) - i) l))



         
instance Codelike Context where
   toCode = undefined  
   -- toCode (ee , _) (Context ty dims) = Right $
   --   "CTypes:\n" ++ (intercalate "\n" (
   --                      -- map (\(nm, val) -> nm ++ " : " ++ show val) ty
   --                      map 
   --                      (\((nm , val),k) ->
   --                           nm ++ " : " ++ (toString (ee , Context (drop k ty) []) val)
   --                           ++ "\n"
   --                           ++ (printCTypeCorners ee (Context ty []) val (length ty - k))
   --                           ++ "\n"
   --                      )
   --                        (zip ty [1..]) 
                        
   --                      ))
   --   ++ "\nDimensions:\n" ++ (intercalate "\n" (map (\(nm, _) -> (fromMaybe "_" nm) ++ " : " ++ "X") dims))
  
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
   show Env = "Env"

type Boundary = [((Int,Bool),Expr)]


indent :: Int -> String -> String
indent i s = intercalate ("\n" ++ (replicate i ' ')) (splitOn "\n" s) 

class Codelike a where
  -- toCode = undefined
  toCode :: (Env , Context) -> a -> Either String String

  toStringEE :: Context -> a -> String
  toStringEE ctx = toString (Env  , ctx) 

  toString :: (Env , Context) -> a -> String
  toString c = either id id . toCode c
  
instance Codelike (Int , Bool) where
  toCode = undefined
  -- toCode (_ , ctx) (i , b) =
  --   do s <- (getDimSymbolAlwaysPrim ctx i)
  --      return ((if b then "" else "~ ")  ++ s)


  
instance Codelike IExpr where
  toCode = undefined
  -- toCode eee x =  
  --    do l <- traverse (traverse (toCode eee)) (elimIExpr id x)
  --       return (intercalate " ∨ " (map (intercalate " ∧ ") l))

parr :: String -> String
parr x = "(" ++ x ++ ")" 

instance Codelike Partial where
  toCode c pa =
     do l <- traverse (\(f , e) -> do bo <- toCode c e
                                      fc <- toCode c f
                                      return ( (parr $ parr fc ++ " = i1") ++ " → " ++ bo ++ "\n")

                       ) (Map.toList pa)
        return (intercalate ";" l)

instance Codelike OExpr where
  toCode = undefined  
instance Codelike Expr where
  toCode = undefined
  -- toCode (ee , c) (HComp v pa e) =
  --    do x <- (toCode (ee , (addDimToContext c v)) pa)
  --       y <- (toCode (ee , c) e)
  --       return ("hcomp " ++ "(λ " ++ fromMaybe "todo" v ++ " → λ { " ++ (indent 5 ("\n" ++ x)) ++ "})\n" ++ parr y )
  -- -- toCode (ee , c) (ILam s e) =
  -- --    do x <- (toCode (ee , (addDimToContext c s)) e)
  -- --       return ("λ " ++ fromMaybe "_" s ++ " → " ++ (indent 5 ("\n" ++ x)) ++ "\n" )
  -- toCode (e , c) (Var (VarIndex h) t) =
  --    do let l = map
  --               (\((e0, e1), a) ->
  --                  -- parr((toString (e , c) e0) ++ "|" ++ (toString (e , c) a)  ++"|"++ (toString (e , c) e1))
  --                   (toString (e , c) a)
  --                  )
                   
  --                     t
  --       hc <- getVarSymbol c h
  --       return (hc ++ " " ++ (intercalate " " (map parr l)))
        
  -- toCode _ (Hole hI) =
  --    return ("{!!}")

instance Codelike LExpr where
  toCode ee ([] , e) = toCode ee e

  toCode (ee , c) (s : ss , e) =
          do x <- (toCode (ee , (addDimToContext c s)) ( ss , e) )
             return ("λ " ++ fromMaybe "_" s ++ " → " ++ (indent 5 ("\n" ++ x)) ++ "\n" )


instance Codelike CType where
  toCode = undefined
  -- -- toCode _ = pure . show
  -- toCode (ee , _) (CType bt []) = getBTypeSymbol ee bt
  -- toCode (ee , ctx) ct@(CType bt ([(e0 , e1) ])) = 
  --     do s0 <- toCode (ee , ctx) e0
  --        s1 <- toCode (ee , ctx) e1
  --        return (s0 ++ " ≡ " ++ s1)
      
  -- -- toCode (ee , ctx) ct@(CType bt (x : xs)) = pure $ show ct
  --   --pure "todo in Syntax: instance Codelike CType" 
  -- toCode (ee , ctx) ct@(CType bt fcs@(x : xs)) = Right $
  --    "CType \n" ++
  --      indent 5 ("\n" ++ (intercalate "\n"
  --               ((zipWith (\k -> \(e0 , e1) ->
  --                               (show k) ++ "=0" ++ "\n" ++ e0 ++ "\n\n"
  --                            ++ (show k) ++ "=1" ++ "\n" ++ e1) [0..])
  --                   $ map (mapBoth (toString (ee , ctx))) fcs)))   
    
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
emptyEnv = Env

emptyCtx :: Context
emptyCtx = Context [] []

freshCtx :: Int -> Context
freshCtx n = foldl addDimToContext emptyCtx (map Just $ genericDims n)

-- mkBType :: Env -> Int -> Either String BType
-- mkBType (Env lN _) bI =
--   if (bI < length lN)
--   then Right (BType bI)
--   else Left "bad level arg"


-- getBaseType :: Env -> Context -> CType -> BType
-- getBaseType _ _ (CType bTy _) = bTy


getCTyDim :: Env -> Context -> CType -> Int
getCTyDim env c (CType e) = getExprDim env c e



-- this tells us arity of expresion without looking into dimensions defined in context
getExprDim :: Env -> Context -> Expr -> Int
getExprDim _ _ (Expr n _) = n
-- getExprDim e c (Var vI tl) = 
--     ((\k -> k - (length tl)) . getCTyDim e c) (getVarType c vI)
-- getExprDim e c (HComp _ _ x) = getExprDim e c x
-- -- getExprDim e c (ILam _ x) = 1 + (getExprDim e c x)
-- getExprDim e c (Hole _) = 0


-- TODO :: add arity checking
-- mkCType :: Env -> Context -> (BType , [(LExpr , LExpr)]) -> Either String CType
-- mkCType e c (bTy , []) = Right (CType bTy []) 
-- mkCType e c (bTy , faces) =
--   let ged = getExprDim e c in
--   -- if ([(length faces - 1 , length faces - 1) ] == nub (map (bimap ged ged ) $ faces ))
--    (Right (CType bTy faces))
--   -- else (Left "faces of wrong dimension")



mkHcomp ::  Env -> Context -> (Maybe String , Partial , Expr) -> Either String OExpr
mkHcomp env c (s , pa , e) =
  Right (HComp s pa e)
  -- if (getExprDim env c e == 0)
  -- then Right (HComp s pa e)
  -- else Left "only 0-dimensional terms are allowed in center of hcomp"

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

toSSnumStr :: Int -> String 
toSSnumStr i = map toSSnum (show i)

toSSnum :: Char -> Char
toSSnum x =
  case TR.readMaybe [x] of
    Just d -> "₀₁₂₃₄₅₆₇₈₉" !! d
    Nothing -> x

fromSSnum :: Char -> Maybe Char
fromSSnum x =
  case elemIndex x "₀₁₂₃₄₅₆₇₈₉" of
    Just d -> Just (head (show d))
    Nothing -> Nothing

      
--"₀₁₂₃₄₅₆₇₈₉"
  
genericDims n = take n $ (["i","j","k"] ++ [ "i" ++ map toSSnum (show i) | i <- range (n - 3) ])
  
-- mkExprGrid :: Int -> Int ->  ((Env , Context) , Expr)
-- mkExprGrid n k = ((env , ctxTop) , meg ctxTop k) 

--   where
--     (env , ctxTop) =
--       -- let dims = fmap (flip (,) Nothing) $ take n (["i","j","k"] ++ [ "i" ++ (show i) | i <- range (n - 3) ])
--         (Env [] [] , Context [] (replicate n (Nothing,Nothing)) )
  
      
--     meg ctx 0 = Hole 0
--     meg ctx k =
--       let newVar = ( "z" ++ (show (k - 1)))
--           newCtx = addDimToContext ctx (Just newVar)
--       in  either ( \e -> error $ e ++ "imposible: " ++ (either id id $ toCode (env , ctxTop) ctx) )
--                  id (mkHcomp env ctx (Just newVar
--                        , Map.fromSet (\sf2 ->
--                                         meg (addSF2ConstraintToContext sf2 newCtx) (k - 1)
--                                      ) (fExprAllFaces ctx)

--                        , meg ctx (k - 1) ))
      


-- -- dimensionsContextPP :: Context -> String
-- -- dimensionsContextPP c@(Context _ l) =
-- --   sywithPlaceholdersDimSymbols
