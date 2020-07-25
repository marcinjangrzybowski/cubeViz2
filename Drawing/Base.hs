{-# LANGUAGE DeriveFunctor #-}
module Drawing.Base where

import Data.Bifunctor

-- import Data.Colour
import Data.Maybe

import Data.List

-- import Data.Floating.Classes


type NPoint = [ Float ]

data Seg = Pt Float | Seg Float Float


type Prll = ( Int , [[ Float ]] )      
                    -- Int=k means number means dimension of paraelogram (not ambient space!)
                    -- length of list on first level is eequal to 2 ^ k
                    -- dimension of ambient space must by consistent with context it is equal to length of lists on second level 
                    
type Shp a = (Prll , a)

    
data Drawing a = Drawing [(Shp a)]
  deriving Functor

data Color = Rgba Float Float Float Float

-- type Settings = ()

-- type DStyle a = (Color , [ Settings ])
    
data MetaColor a = MShape [a] | SShape [a] | Mask
   deriving Functor

type DrawingGL = Drawing (MetaColor Color)


drawingSplitBase :: Drawing (MetaColor a) -> [ (Prll , Maybe Prll , [a])  ] 
drawingSplitBase = drawingSplitStep Nothing
  where

    drawingSplitStep :: Maybe Prll -> Drawing (MetaColor a) -> [ (Prll , Maybe Prll , [a])  ] 
    drawingSplitStep m (Drawing ls) = 
      case ls of
        [] -> []
        ((p , x) : xs) ->
          case (m , x)  of
             (Just mp , MShape a) ->
                 ( p , Just mp , a ) : drawingSplitStep (Just mp) (Drawing xs)
             (_ , SShape a) ->
                 ( p , Nothing , a ) : drawingSplitStep Nothing (Drawing xs)
             (_ , Mask ) -> drawingSplitStep (Just p) (Drawing xs)
             (_ , _) -> drawingSplitStep Nothing (Drawing xs)
    
         
-- pointR = 1    


-- toShape :: [Seg] -> Maybe Shape
-- toShape l =
--     case l of
--         [Pt x , Pt y] -> Just (Circle (x , y) pointR)
--         [Seg x0 x1 , Seg y0 y1] -> Just (Rect ( x0 , y0 ) ( x1 - x0) (y1 - y0) )
--         [Seg x0 x1 , Pt y] -> Just (Path ( x0 , y ) [ LineTo ( x1 , y ) ])
--         [Pt x , Seg y0 y1] -> Just (Path ( x , y0 ) [ LineTo ( x , y1 ) ])     --         _ -> Nothing             
    
-- toRenderable : Shp MetaColor -> Maybe (Renderable , Int)
-- toRenderable ( (n , l) , a ) =
--     (case l of
--         [ [x , y] ] ->  Just ((circle (x , y) pointR , 0))
--         [ [x0 , y0] , [x1 , y1] ] -> Just ((path ( x0 , y0 ) [ lineTo ( x1 , y1 ) ]) , 1)
--         [ [x0 , y0] , [x1 , y1] ,  [x2 , y2] ,  [x3 , y3] ]
--              -> Just ((path ( x0 , y0 ) [ lineTo ( x1 , y1 )
--                                        , lineTo ( x3 , y3 )
--                                        , lineTo ( x2 , y2 ) ]), 2)
--         _ ->
--              -- let ww = log ("wrong dim draw: " ++ (String.fromInt n)) (l , a) in
--              Nothing   
--     )    
--     |> Maybe.map (Tuple.mapFirst (
--                   List.singleton
--                   >> (
--                       let op = choose (n == 1) fill stroke
--                       in
--                          a |> Maybe.map (\(b , c) ->
--                                         List.append [compositeOperationMode
--                                            (choose b
--                                             SourceOver DestinationOver)
--                                          , c |> Tuple.first |> op ]
--                                             (c |> Tuple.second)
--                                       ) 
--                          |> Maybe.withDefault ([compositeOperationMode DestinationOut
--                                                 , op Color.black])
--                          |> (List.append [lineWidth 1 ]) 
--                          |> shapes


--                        -- if n == 1
--                        -- then shapes [ stroke Color.black ] --(colFn a) ]
--                        -- else shapes [ fill (colFn a)  ]   
--                      )
--                  )) 
               
-- toRenderableAll : Drawing MetaColor -> List Renderable    
-- toRenderableAll l =
--     let allR = List.filterMap (toRenderable) l in
--     List.concat [(List.filter (Tuple.second >> isEqual 2) allR)      
--                 ,(List.filter (Tuple.second >> isEqual 1) allR)
--                 ,(List.filter (Tuple.second >> isEqual 0) allR)]    
--     |> List.map (Tuple.first)                


metaTint :: b -> Drawing a -> Drawing b                   
metaTint = fmap . const                  




-- mapCoords : ((Int -> Float) -> (Int -> Float)) -> Drawing a -> Drawing a
-- mapCoords f = 
--       List.map ( ambFnOnArr f)     
--     |> Tuple.mapSecond
--     |> Tuple.mapFirst
--     |> List.map   
                 
mapCoords :: ([Float] -> [Float]) -> Drawing a -> Drawing a
mapCoords f (Drawing l) =
   Drawing (map (first (second (map f))) l)
           
-- pxScale : (Int , Int) -> Drawing a -> Drawing a
-- pxScale (w , h) =
--     List.map (Tuple.mapFirst (Tuple.mapSecond ( List.map ( 
--                \(l) ->
--                    case l of
--                        [x , y] -> [x * (toFloat w) , y * (toFloat h)]
--                        _ -> l
--                               ))))
-- -- shapes [ fill (color) ]
-- --                                             [ rect (x * w , y * h) (w / frs) (h / frs) ]


-- segSplitIso : Iso (Seg) ((Float , Float) , Bool)      
-- segSplitIso =
--     ((\x -> case x of                     
--              Pt p -> ((p , p ) , False)
--              Seg p0 p1 -> ((p0 , p1 ) , True)) 
--       ,
--      ( \((pA , pB) , b) -> if b
--                            then (Seg pA pB)
--                            else (Pt pA)    )
--     )


combineDrawings :: [Drawing a] -> Drawing a
combineDrawings = Drawing . concat . (map (\(Drawing x) -> x))


                  
-- combineDrawingsSafe : List (Drawing a) -> Result String (Drawing a)
-- combineDrawingsSafe = combineDrawings >>
--          (\l ->
--               if (l |> List.map (Tuple.first >> prllDim) |> allSame)
--               then (Ok l)
--               else (Err "cannot combine drawings of diferent dimensions")
--          )
                  
-- -- shpSplitIso : Iso (List (Seg)) (List ((Float , Float) , Bool)      
-- -- shpSplitIso =

-- embedPrll: Int -> (List (Float) -> Float) -> Prll -> Prll
-- embedPrll k f = Tuple.mapSecond (List.map (\l -> listInsert k (f l) l))
    
-- embed : Int -> (List (Float) -> Float) -> Drawing a -> Drawing a
-- embed k f = mapCoordsAsL (\l -> listInsert k (f l) l)  

ptZero :: Prll
ptZero = (0 , [[]] )
         
segOne :: Prll
segOne =  (1 , [[0] , [1]] )
         
unitHyCube :: Int -> Prll
unitHyCube 0 = ptZero
unitHyCube 1 = segOne
unitHyCube n =
  let (_ , prev) = unitHyCube (n - 1)
  in (n , (map (0:) prev) ++ (map (1:) prev))

    -- (iter ( (\(k , x)  -> (k + 1
    --               , concat [
    --                      (List.map (listInsert 0 0.0) x)
    --                    , (List.map (listInsert 0 1.0) x)
    --                   ]))) ptZero)                


-- -- it not changes dim of ambient space!        
-- sidePrll : Bool -> Prll -> Prll
-- sidePrll b (n , l) =
--     if n == 0
--     then ptZero
--     else ( (n - 1)  , (choose b List.take List.drop) ( 2 ^ (n - 1)) l
--             )      


        
-- -- it not changes dim of ambient space!
-- fillPrll : Int -> Prll -> Prll -> Prll
-- fillPrll k (n , p) = fillPrllUS (min k n) (n , p)

-- fillPrllUS : Int -> Prll -> Prll -> Prll
-- fillPrllUS k =
--     case k of
--            0 -> \(n , l1) -> \(_ , l2) ->
--                 (n + 1 , List.append l1 l2)
--            _  -> (\p0 -> \p1 -> 
--                fillPrll 0
--                (fillPrll (k - 1) (sidePrll False p0) (sidePrll False p1)) 
--                (fillPrll (k - 1) (sidePrll True p0) (sidePrll True p1))
--                )       
-- -- it not changes dim of ambient space!

-- extrudeDrawingBi : List Float -> Drawing a -> Drawing a
-- extrudeDrawingBi v drw =  
--   zip (translate (List.map (\x -> x * -1) v) drw , translate v drw)
--     |> List.map (\((s , a) , (ns , _)) ->

--              (fillPrll 0 s ns , a)
--                 )
                   
-- extrudeDrawing : List Float -> Drawing a -> Drawing a
-- extrudeDrawing v drw =
--   let newFace = translate v drw
--   in zip (drw , newFace)
--     |> List.map (\((s , a) , (ns , _)) ->

--              (fillPrll 0 s ns , a)
--                 )
--     -- (Tuple.mapFirst             
--     -- (\x -> fillPrll k
--     --    (embedPrll k (const 0) x)
--     --    (embedPrll k (const 1) x) 
--     -- ))    
-- -- 

-- -- it not changes dim of ambient space!        
-- facePrll : Face -> Prll -> Prll                 
-- facePrll (i , b) =
--     case i of
--         0 -> sidePrll b
--         _ -> (\x -> fillPrll 0
--              (facePrll (i - 1 , b) (sidePrll False x) )
--              (facePrll (i - 1 , b) (sidePrll True x) )
--              )    
-- -- subPrll : SubFace -> Prll -> Prll         

prllDim :: Prll -> Int
prllDim = length . head . snd



  -- Tuple.second >> List.head >> Maybe.map (List.length) >> Maybe.withDefault 0

getDrawingDim :: Drawing a -> Maybe Int
getDrawingDim (Drawing ((p , _) : _)) = Just (prllDim p)
getDrawingDim (Drawing _) = Nothing              
-- drawFaces : Prll -> (Face -> a) -> Drawing a 
-- drawFaces prl f = pairFrom ((swap facePrll) prl)  f
--                   |> (tabulateFaces (prllDim prl))
--                   |> List.map (Tuple.second)

-- degenDrawing : Int -> Drawing a -> Drawing a
-- degenDrawing k = List.map
--     (Tuple.mapFirst             
--     (\x -> fillPrll k
--        (embedPrll k (const 0) x)
--        (embedPrll k (const 1) x) 
--     ))                       

-- emptyDrawing : Drawing a    
-- emptyDrawing = []    
    
-- stripesFromList : (a , a) -> List Float -> Drawing a
-- stripesFromList (a0, a1) =
--     listTuples
--     >> List.indexedMap (\i -> \(xA , xB) -> ((1 , [[xA] , [xB]]) , choose (oddQ i) a0 a1))  

-- fromLSeg : List Seg -> Prll
-- fromLSeg = List.foldr (\s ->
--               case s of
--                   Pt x -> embedPrll 0 (const x)
--                   Seg x y -> \z -> fillPrll 0
--                            (embedPrll 0 (const x) z)
--                            (embedPrll 0 (const y) z)
--               ) ptZero             

-- spanPrll : Int -> Float -> Float -> Prll
-- spanPrll n x0 x1 = List.repeat n (Seg x0 x1) |> fromLSeg 

-- segmentsRange : Int -> Float -> Float -> List (Float,Float)
-- segmentsRange n x0 xN =
--     List.range 0 n  
--     |> List.map (\k -> interp x0 xN ((toFloat k) / (toFloat n)))
--     |> listTuples
                     
-- unmasked : Drawing DStyle -> Drawing MetaColor
-- unmasked = mapDrawingData (Tuple.pair False >> Just)           

-- masked : Drawing a -> Drawing (DStyle) -> Drawing MetaColor
-- masked m x =
--       -- case getDrawingDim m of
--       --     Just 1 -> unmasked x
--       --     _ -> 
--              List.append
--              (mapDrawingData (const Nothing) m)
--              (mapDrawingData (Tuple.pair True >> Just) x)                     

-- setLightness : Float -> Color -> Color
-- setLightness lV = toHsla >> (\x -> {x | lightness = lV }) >> fromHsla

-- setSaturation : Float -> Color -> Color
-- setSaturation lV = toHsla >> (\x -> {x | saturation = lV }) >> fromHsla                            

-- mapColor : (Color -> Color) -> Drawing MetaColor -> Drawing MetaColor
-- mapColor = Tuple.mapFirst >> Tuple.mapSecond >>  Maybe.map >> mapDrawingData            




translate ::  [ Float ] -> Drawing a -> Drawing a
translate vec = mapCoords (zipWith (+) vec)

-- translate vec =
--     mapCoordsAsL (\cl -> zip (cl , vec) |> List.map (\(x , delta)-> x + delta ))

scale :: Float -> Drawing a -> Drawing a
scale factor = mapCoords (map (*factor)) 


mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]



mod1 :: Float -> Float
mod1 x = x - fromIntegral (floor x)

hsv :: Float -> Float -> Float -> Color
hsv h s v = case hi of
    0 -> Rgba v t p 1.0
    1 -> Rgba q v p 1.0
    2 -> Rgba p v t 1.0
    3 -> Rgba p q v 1.0
    4 -> Rgba t p v 1.0
    5 -> Rgba v p q 1.0
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

phiNumber :: Float
phiNumber = 1.618033988

nthColor :: Int -> Color
nthColor i = hsv (phiNumber * fromIntegral i * 360.0) 0.5 0.5

debugRainbow :: Drawing (MetaColor a) -> DrawingGL
debugRainbow (Drawing l) = 
   Drawing ( mapInd g $ mapMaybe f l)

   where      
     f (p , SShape a) = Just (p , SShape [()]) 
     f (p , _ ) = Nothing

     g (p , _) i = (p , SShape [ (nthColor i) ])
