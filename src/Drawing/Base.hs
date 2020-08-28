{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Drawing.Base where

import Data.Bifunctor
import Data.Tuple
-- import Data.Colour
import Data.Maybe

import Data.List

-- import Data.Floating.Classes

import Combi

-- type NPoint = [ Float ]

-- data Seg = Pt Float | Seg Float Float


class InSpace a where
  sMap :: ([Float] -> [Float]) -> a -> a


instance {-# OVERLAPPING #-} InSpace [Float] where
  sMap = id

instance (InSpace b)  => InSpace [b] where
  sMap = fmap . sMap



-- instance Bi a => InSpace (a [ Float ]) where
--   sMap = fmap   

type Prll = ( Int , [[ Float ]] )     
                    -- Int=k means number means dimension of paraelogram (not ambient space!)
                    -- length of list on first level is eequal to 2 ^ k
                    -- dimension of ambient space must by consistent with context it is equal to length of lists on second level 

type Smplx = ( Int , [[ Float ]] )     
                    -- Int=k means number means dimension of simplex (not ambient space!)
                    -- length of list on first level is eequal to (k + 1)
                    -- dimension of ambient space must by consistent with context it is equal to length of lists on second level 

instance (InSpace b)  => InSpace (Int , b) where
  sMap = second . sMap



type Mask = [ Smplx ] 

type Shp a = (Prll , a)
  -- deriving (Functor)

instance InSpace (Shp a) where
  sMap f = first (sMap f)


newtype Drawing a = Drawing [(Either (Shp a) (Mask , [(Shp a)])) ]
  deriving (Show)

instance InSpace (Drawing a) where
  sMap q (Drawing l) = Drawing $ fmap (bimap (sMap q) (bimap (sMap q) (sMap q))) l 

instance Functor Drawing where 
  fmap f (Drawing l) = Drawing $ fmap (bimap (fmap f) (second $ fmap (fmap f))) l


data Color = Rgba Float Float Float Float
  deriving (Show)
            

-- toMask :: Shp (MetaColor a) -> Maybe (Shp (MetaColor b))
-- toMask (p , Mask) = Just (p , Mask)
-- toMask _ = Nothing

masked :: Mask -> [(Shp a)] -> Drawing a
masked p l = Drawing [Right (p , l)] 

unmasked :: [(Shp a)] -> Drawing a
unmasked = Drawing . fmap Left 



-- demask :: Drawing (MetaColor a) -> Drawing a
-- demask (Drawing l) =
--   Drawing $
--     concat (map (\x ->
--                    case x of
--                      (_ , Mask) -> []
--                      (s , MShape y) -> [(s , y)]
--                      (s , SShape y) -> [(s , y)]
--                    ) l)  

           

gray x = Rgba x x x 1.0 

-- type Settings = ()

-- type DStyle a = (Color , [ Settings ])
    
-- data MetaColor a = MShape a | SShape a | Mask
--    deriving (Functor , Show)

type DrawingGL = Drawing Color

class Colorlike a where
  toColor :: a -> Color

  toDrawingGL :: Drawing a -> DrawingGL
  toDrawingGL = (fmap toColor)

instance Colorlike Color where
  toColor = id

instance Colorlike (Color , b) where
  toColor = fst

instance Colorlike (b , Color) where
  toColor = snd

instance Colorlike [Color] where
  toColor = head

instance Colorlike () where
  toColor _ = (Rgba 0.5 0.5 0.5 1.0 )

drawingSplitBase :: Drawing a -> [ (Prll , Maybe Smplx , a)  ] 
drawingSplitBase = undefined
--   where

--     drawingSplitStep :: Maybe Prll -> Drawing (MetaColor a) -> [ (Prll , Maybe Prll , a)  ] 
--     drawingSplitStep m (Drawing ls) = 
--       case ls of
--         [] -> []
--         ((p , x) : xs) ->
--           case (m , x)  of
--              (Just mp , MShape a) ->
--                  ( p , Just mp , a ) : drawingSplitStep (Just mp) (Drawing xs)
--              (_ , SShape a) ->
--                  ( p , Nothing , a ) : drawingSplitStep Nothing (Drawing xs)
--              (_ , Mask ) -> drawingSplitStep (Just p) (Drawing xs)
--              (_ , _) -> drawingSplitStep Nothing (Drawing xs)
    
    
metaTint :: b -> Drawing a -> Drawing b                   
metaTint = fmap . const                  

-- ss :: ([Float] -> [Float]) -> Drawing a -> Drawing a
-- ss = sMap

                 
mapCoords :: ([Float] -> [Float]) -> Drawing a -> Drawing a
mapCoords = sMap
--    Drawing (map (first (second (map f))) l)
           


combineDrawings :: [Drawing a] -> Drawing a
combineDrawings = Drawing . concat . (map (\(Drawing x) -> x))


                 
-- this increments dimension of Prll, but not ambient dimension!!

extrudePrll :: [Float] -> Prll -> Prll
extrudePrll _ (_ , [] ) = error "extrude - bad Prll"
extrudePrll v (k , xs@(x : _) ) =
  if (length v == length x)
  then (if (length v == k)
        then error "extrude cannot be performed on Prll of codim=0"
        else (k + 1 , xs ++ (fmap (zipWith (+) v) xs))
       )
  else error "extrude vector dimension do not match Prll"


-- extrudeNotMeta :: [Float] -> Drawing a -> Drawing a
-- extrudeNotMeta v (Drawing l) =
--   Drawing $
--     ( fmap (first $ extrudePrll v) l )

-- extrude :: [Float] -> Drawing (MetaColor a) -> Drawing (MetaColor a)
-- extrude v (Drawing l) =
--   Drawing $
--     ( fmap
--       (\(p , c) ->
--          case c of
--            Mask -> undefined
--            MShape a -> undefined
--            SShape a -> (extrudePrll v p , SShape a)
--              )
--       l )

versor :: Int -> Int -> [Float]
versor n k = fmap (\i -> if i == k then 1.0 else 0.0) $ range n
       
embed :: Int -> ([ Float ] -> Float) -> Drawing a -> Drawing a
embed k f = mapCoords (\l -> listInsert k (f l) l)  


transposeDrw :: Int -> Drawing a -> Drawing a
transposeDrw k = mapCoords (\l -> listInsert k (last l) (init l)) 

-- addDim :: Int -> (Float,Float) -> Drawing a -> Drawing a
-- addDim i (x0 , x1) d =
--   case (getDrawingDim d) of
--     Nothing -> emptyDrawing
--     Just n ->  extrudeNotMeta (listInsert i (x1 - x0) $ replicate n 0) $ embed i (const x0) d

-- addDimPRL :: Int -> (Float,Float) -> Prll -> Prll
-- addDimPRL i x d =
--   case (addDim i x (Drawing [(d , ())])) of
--     Drawing [(d2 , _)] -> d2


-- addDimL :: Int -> [(Float,Float)] -> Drawing a -> Drawing a
-- addDimL i l d = combineDrawings $ fmap (flip (addDim i) d)  l 


-- ptZero :: Prll
-- ptZero = (0 , [[]] )
         
-- segOne :: Prll
-- segOne =  (1 , [[0] , [1]] )


-- grid :: Int -> Int -> a -> Drawing a
-- grid 0 n a = Drawing [(ptZero , a)]
-- grid k n a = addDimL 0 l $ grid (k - 1) n a
--   where
--     l :: [(Float,Float)]
--     l = map (\x -> (((fromIntegral x) / (fromIntegral n))
--                     , ((fromIntegral x) / (fromIntegral n))  + 0.1

--                     )) $ range n
          
-- unitHyCube :: Int -> Prll
-- unitHyCube 0 = ptZero
-- unitHyCube 1 = segOne
-- unitHyCube n =
--   let (_ , prev) = unitHyCube (n - 1)
--   in (n , (map (0:) prev) ++ (map (1:) prev))

--     -- (iter ( (\(k , x)  -> (k + 1
--     --               , concat [
--     --                      (List.map (listInsert 0 0.0) x)
--     --                    , (List.map (listInsert 0 1.0) x)
--     --                   ]))) ptZero)                



-- prllDim :: Prll -> Int
-- prllDim = length . head . snd



--   -- Tuple.second >> List.head >> Maybe.map (List.length) >> Maybe.withDefault 0

-- getDrawingDim :: Drawing a -> Maybe Int
-- getDrawingDim (Drawing ((p , _) : _)) = Just (prllDim p)
-- getDrawingDim (Drawing _) = Nothing              

-- emptyDrawing :: Drawing a    
-- emptyDrawing = Drawing []    
    




-- translate ::  [ Float ] -> Drawing a -> Drawing a
-- translate vec = mapCoords (zipWith (+) vec)


-- scale :: Float -> Drawing a -> Drawing a
-- scale factor = mapCoords (map (*factor)) 


-- mapInd :: (a -> Int -> b) -> [a] -> [b]
-- mapInd f l = zipWith f l [0..]



-- mod1 :: Float -> Float
-- mod1 x = x - fromIntegral (floor x)

-- hsv :: Float -> Float -> Float -> Color
-- hsv h s v = case hi of
--     0 -> Rgba v t p 1.0
--     1 -> Rgba q v p 1.0
--     2 -> Rgba p v t 1.0
--     3 -> Rgba p q v 1.0
--     4 -> Rgba t p v 1.0
--     5 -> Rgba v p q 1.0
--  where
--   hi = floor (h/60) `mod` 6
--   f = mod1 (h/60)
--   p = v*(1-s)
--   q = v*(1-f*s)
--   t = v*(1-(1-f)*s)

-- phiNumber :: Float
-- phiNumber = 1.618033988

-- nthColor :: Int -> Color
-- nthColor i = hsv (phiNumber * fromIntegral i * 360.0) 1.0 0.5


-- extractMasks :: Drawing (MetaColor a) -> Drawing (MetaColor ())
-- extractMasks (Drawing l) = 
--   Drawing $ map (second (const (SShape ())))  $ catMaybes $ map toMask l  

-- debugRainbow :: Drawing (MetaColor a) -> DrawingGL
-- debugRainbow (Drawing l) = 
--    Drawing ( mapInd g $ mapMaybe f l)

--    where      
--      f (p , SShape a) = Just (p , SShape [()]) 
--      f (p , _ ) = Nothing

--      g (p , _) i = (p , SShape (nthColor i))
