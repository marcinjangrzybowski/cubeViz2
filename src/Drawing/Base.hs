{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE FlexibleInstances #-}

module Drawing.Base where

import Data.Bifunctor
import Data.Tuple
-- import Data.Colour
import Data.Maybe

import Data.List
import Data.Bool
-- import Data.Floating.Classes

import PiecesEval

import Combi

import Drawing.Color

import DataExtra

import Control.Applicative

import Debug.Trace

import qualified Data.Map as Map

-- type NPoint = [ Float ]

-- data Seg = Pt Float | Seg Float Float


class InSpace a where
  sMap :: ([Float] -> [Float]) -> a -> a

  scale :: Float -> a -> a
  scale x = sMap (fmap (x *))

  translate :: [Float] -> a -> a
  translate x = sMap (zipWith (+) x)

  translatePar :: [Float] -> Float -> a -> a
  translatePar x t = sMap (zipWith (+) (map (* t) x))

  scaleNonUniform :: [Float] -> a -> a
  scaleNonUniform x = sMap (zipWith (*) x)

  scaleOrigin :: Float -> [Float] -> a -> a
  scaleOrigin s o = translate o . scale s  . translate ((map negate) o)

  scaleNonUniformOrigin :: [Float] -> [Float] -> a -> a
  scaleNonUniformOrigin x o = translate o . scaleNonUniform x . translate ((map negate) o)


instance {-# OVERLAPPING #-} InSpace [Float] where
  sMap = id

instance (InSpace b)  => InSpace [b] where
  sMap = fmap . sMap

instance InSpace a  => InSpace (a , b) where
  sMap = first . sMap

-- instance Bi a => InSpace (a [ Float ]) where
--   sMap = fmap   



--todo Smplx should be non empty!
type Smplx = [[ Float ]]


-- type MetaColor = [Maybe Color]


type Drawing a = [(Smplx , a)]

type DrawingGL = Drawing Color

type Pt3D = (Float , Float , Float)


data Renderable = Point Pt3D | Line (Pt3D , Pt3D) | Triangle (Pt3D , Pt3D , Pt3D)


type Renderables = [(Renderable,Shade)]





toRenderable :: Smplx -> Maybe Renderable
toRenderable [([ x0 , y0 , z0 ]) , ([ x1 , y1 , z1]) , ([ x2 , y2 , z2])] =
    Just $ Triangle ((x0 , y0 , z0) , (x1 , y1 , z1) , (x2 , y2 , z2))
toRenderable [([ x0 , y0 , z0 ]) , ([ x1 , y1 , z1])] = Just $ Line ((x0 , y0 , z0) , (x1 , y1 , z1))
toRenderable [[ x0 , y0 , z0 ]] = Just $ Point (x0 , y0 , z0)
toRenderable [([ x0 , y0]) , ([ x1 , y1]) , ([ x2 , y2])] =
    Just $ Triangle ((x0 , y0 , 0) , (x1 , y1 , 0) , (x2 , y2 , 0))
toRenderable [([ x0 , y0 ]) , ([ x1 , y1 ])] = Just $ Line ((x0 , y0 , 0) , (x1 , y1 , 0))
toRenderable [[ x0 , y0 ]] = Just $ Point (x0 , y0 , 0)
toRenderable _ = Nothing


-- when aproching Simplex of too high dimension, recurenty takes skeletons until land on renderable symplexes

toRenderableForce :: Smplx -> [Renderable]
toRenderableForce l =
  maybe (explode l >>= toRenderableForce) pure (toRenderable l)

data DrawingInterpreter a =
  DrawingInterpreter
   { ifEmpty :: Renderables
   , fromDrawing :: Int -> Maybe (Drawing a -> Renderables)
   }

simplexDim ::  Smplx -> Int
simplexDim x = length $ head x


drawingDim ::  Drawing a -> Maybe Int
drawingDim [] = Nothing
drawingDim ((x , _) : _) = Just (length (head x))

traceDim :: Drawing a -> Drawing a
traceDim x = trace
    ("zz: " ++ show (fmap (simplexDim . fst) x))
    x

toRenderableDI :: Shadelike a => DrawingInterpreter a -> Drawing a -> Either String (Maybe Int , Renderables)
toRenderableDI di dr =
  case ensureFold (length . head . fst) dr of
    EFREmpty -> Right (Nothing , ifEmpty di)
    EFREvery dim -> case fromDrawing di dim <*> pure dr of
                       Nothing -> Left ("not implementedDimInDrawingInterpreter " ++ show dim)
                       Just re -> Right (Just dim , re)
    _ -> error "mixed dimensions drawing"

mapStyle :: (a -> a) -> Drawing a -> Drawing a
mapStyle = fmap . fmap

-- throws error when aproching unrenderable
toRenderables :: Shadelike a => Drawing a -> Renderables
toRenderables = fmap $ bimap (fromJust . toRenderable) toShade

toRenderablesIgnore :: Shadelike a => Drawing a -> Renderables
toRenderablesIgnore =  mapMaybe (\(x , y) -> fmap (flip (,) y) x ) . (fmap $ bimap toRenderable toShade)



toRenderablesForce :: Shadelike a => Drawing a -> Renderables
toRenderablesForce l =
  l >>= (\(l , a) -> fmap (flip (,) a) l) . (bimap toRenderableForce toShade)

transposeDrw :: Int -> Drawing a -> Drawing a
transposeDrw k = sMap (\l ->
                         case l of
                           [] -> error "attempt to tranpose 0 dimensional drawing"
                           _ -> listInsert k (last l) (init l))



versor :: Int -> Int -> [Float]
versor n k = (\i -> if i == k then 1.0 else 0.0) <$> range n

embed :: Int -> ([ Float ] -> Float) -> Drawing a -> Drawing a
embed k f = sMap (\l -> listInsert k (f l) l)

embedSF :: SubFace -> Drawing a -> Drawing a
embedSF (SubFace n mp) =
  let z = map ( \(k , b)  -> embed k (const (if b then 1.0 else 0.0))) (Map.toList mp)
  in foldFL z

-- extrudeNotMeta :: [Float] -> Drawing a -> Drawing a
-- extrudeNotMeta v (Drawing l) =
--   Drawing $
--     ( fmap (first $ extrudePrll v) l )

data ExtrudeMode = Basic | ExtrudeLines | ExtrudePlanes | Midpoints

extrudeBasicF :: Int -> ([Float] -> Float , [Float] -> Float) -> (Smplx , a) -> Drawing a
extrudeBasicF k (f0 , f1) (s , a) =
    -- (=<<) (\(s , a) ->
     let ss = map
               (\i ->
                 let s0 = (\pt -> listInsert k (f0 pt) pt) <$> drop i s
                     s1 = (\pt -> listInsert k (f1 pt) pt) <$> take (i + 1) s
                 in s0 ++ s1
                  )
               $ fst <$> zip [0..] s
     in map ((, a)) ss
   
extrudeLinesF :: Int -> ([Float] -> Float , [Float] -> Float) -> (Smplx , a) -> Drawing a
extrudeLinesF k (f0 , f1) (s , a) =
    -- (=<<) (\(s , a) ->
     let ss = if length s > 2 then error "dim>1 feeded to extrudeLinesF!!"
              else map (\pt -> listInsert k (f0 pt) pt  ) s
                     :
                     map (\pt -> listInsert k (f1 pt) pt  ) s
                     :
                     map (\pt -> [listInsert k (f0 pt) pt , listInsert k (f1 pt) pt ]  ) s

              -- map
              --  (\i ->
              --    let s0 = fmap (\pt -> listInsert k (f0 pt) pt) $ take (i + 1) s
              --        s1 = fmap (\pt -> listInsert k (f1 pt) pt) $ drop i s
              --    in s0 ++ s1
              --     )
              --  $ fmap fst $ zip [0..] s

     in map ((, a)) ss

extrudeMidpointsF :: Int -> ([Float] -> Float , [Float] -> Float) -> (Smplx , a) -> Drawing a
extrudeMidpointsF k (f0 , f1) ([ pt ] , a) =
   [([listInsert k ((f0 pt + f1 pt) * 0.5) pt] , a)]

extrudeMidpointsF _ _ _ = error "only points are expected for this Midpoints extrudemode"
  
extrudePlanesF :: Int -> ([Float] -> Float , [Float] -> Float) -> (Smplx , a) -> Drawing a
extrudePlanesF k (f0 , f1) (s , a) =
    -- (=<<) (\(s , a) ->
     undefined

extrudeF :: ExtrudeMode -> (Int -> ([Float] -> Float , [Float] -> Float) -> (Smplx , a) -> Drawing a)
extrudeF Basic = extrudeBasicF
extrudeF ExtrudeLines = extrudeLinesF
extrudeF ExtrudePlanes = extrudePlanesF
extrudeF Midpoints = extrudeMidpointsF

class Extrudable a where
  extrudeMode :: a -> ExtrudeMode
  extrudeMode _ = Basic

  extrudeFPriv :: Int -> ([Float] -> Float , [Float] -> Float) -> (Smplx , a) -> Drawing a
  extrudeFPriv k f (s , a) = extrudeF (extrudeMode a) k f (s , a)

  extrude :: Int -> ([Float] -> Float , [Float] -> Float) -> Drawing a -> Drawing a
  extrude k f = (=<<) $ extrudeFPriv k f

instance Extrudable ([String],Color) where

type MColor = (([String],ExtrudeMode),Color)

instance Extrudable (( a ,ExtrudeMode),Color) where
  extrudeMode = snd . fst


--first arg is dim of ambient, second dim of simplex
mapWithDim :: (Int -> Int -> (Smplx , a) -> [(Smplx , a)]) -> Drawing a -> Drawing a
mapWithDim _ [] = []
mapWithDim f xs =
 xs >>= (\(pts , a) -> f (length (head pts) ) (length pts - 1) (pts , a) )

getSkel :: Int -> Drawing a  -> Drawing a
getSkel sk l = undefined

extrudeSkel :: Int -> Int -> ([Float] -> Float , [Float] -> Float) -> Drawing a -> Drawing a
extrudeSkel k sk (f0 , f1) = undefined

getPoints :: Drawing a  -> Drawing a
getPoints l = undefined

extrudeLines :: Int -> Int -> ([Float] -> Float , [Float] -> Float) -> Drawing a -> Drawing a
extrudeLines k sk (f0 , f1) = undefined




type ZDrawing a = FromLI Piece (Drawing a)

type ColorType = (([String] , ExtrudeMode) , Color)

type ColorType2 a  = ((a , ExtrudeMode) , Color)





-- dim is dimention of input,
pieceHyperPlanes :: (Bool , Int) -> Piece -> ([Float] -> Float , [Float] -> Float)
pieceHyperPlanes (b , j0) pc0@( Subset dim _ , _) = ( hyperPlanes !! j , hyperPlanes !! (j + 1) )

  where

    pc = pc0

    hyperPlanes0 =
       sortBy (pieceComp pc)
       (fmap (flip (,) False) (range dim) ++ fmap (flip (,) True) (range dim))


    hyperPlanes1 :: [[Float] -> Float]
    hyperPlanes1 =
       let (hF , hT) = halves $ fmap (\ (k , bb) pt -> evalNegFloat bb (pt !! k) )  hyperPlanes0
       in
       [const 0]
         ++
          hF
         ++
       [const 0.5]
         ++
         hT
         ++
       [const 1.0]

    hyperPlanes =
       if b then reverse hyperPlanes1 else hyperPlanes1

    j = dim - j0

degen :: (Extrudable a) => Int -> ZDrawing a -> ZDrawing a
degen k (FromLI n f) =

   -- fmap (scaleRelToCenter 1.01)

   FromLI (n + 1)
     (\pc ->
        let ((crnr , side) , pcPrj ) = projectSplit k ( pc)
        in extrude k (pieceHyperPlanes (crnr , side) pcPrj) (f pcPrj) )




centerOf :: Smplx -> [Float]
centerOf [] = error "illegal simplex"
centerOf pts = map average $ transpose pts



scaleRelToCenter :: Float -> Drawing a -> Drawing a
scaleRelToCenter f =
  map $ first
    (\pts ->
       let center = centerOf pts
       in scaleOrigin f center pts
     )



degenAll :: (Extrudable a) => [Int] -> ZDrawing a -> ZDrawing a
degenAll ds x = foldl (flip degen) x ds


extrudeSeg :: Drawing a -> Drawing a
extrudeSeg =
  (=<<) (\(s , a) ->
     let ss = map
               (\i ->
                 let s0 = ((:) 0) <$> take (i + 1) s
                     s1 = ((:) 1) <$> drop i s
                 in s0 ++ s1
                  )
               $ fst <$> zip [0..] s

     in map ((, a)) ss)

-- todo inline!
unitHyCube :: Int -> Drawing ()
unitHyCube 0 = [([[]] , ())]
unitHyCube n = extrudeSeg (unitHyCube (n - 1))


scaleCell :: Float -> Drawing a -> Drawing a
scaleCell f = scaleOrigin f (repeat 0.5)

unitHyCubeSkel :: Int -> Int -> Drawing ()
unitHyCubeSkel _ k | k < 0
                   = []
unitHyCubeSkel 0 _ = [([[]] , ())]
unitHyCubeSkel n k =
  if k >= n
  then unitHyCube n
  else
        embed 0 (const 0) (unitHyCubeSkel (n - 1) k)
          ++
        embed 0 (const 1) (unitHyCubeSkel (n - 1) k)
          ++
        extrudeSeg (unitHyCubeSkel (n - 1) (k - 1))



fattenSmplx :: Float -> Smplx -> [Smplx]
fattenSmplx a [x] =
  let dim = (length x)
  in fst <$> (translate x $ scale a $ translate (replicate dim (-0.5) ) $ unitHyCubeSkel dim  (min dim 2))
  
fattenSmplx _ x = [x]


fattenOn :: (a -> Bool) -> Float -> (a -> a) -> Drawing a -> Drawing a
fattenOn test x s =
  concatMap (\d -> 
              if (not $ test $ snd d)
              then [d]
              else (, s (snd d)) <$> fattenSmplx x (fst d)

            )
  

