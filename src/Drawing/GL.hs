module Drawing.GL where


import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue
                                  , newTVarIO)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment

import LoadShaders
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Drawing.Color
import Drawing.Base as D
import Drawing.Example

import Data.List

import Data.Maybe

import Data.Bifunctor

import DataExtra

import Debug.Trace

data Descriptor = Descriptor
  { dPrimitiveMode :: PrimitiveMode
  , dVertexArrayObject :: VertexArrayObject
  , dArrayIndex :: ArrayIndex
  , dNumArrayIndices :: NumArrayIndices
  , dLineWidth :: Int
  }
  deriving Show


data CombinedVertexData =
  CombinedVertexData { pointVs :: [GLfloat] , lineVs :: [GLfloat] , triangleVs :: [GLfloat] }

emptyCVD = CombinedVertexData { pointVs = [] , lineVs = [] , triangleVs = [] }


defaultLineWidth = 2

perVert :: [[a]] -> [a] -> [a]
perVert lv lt = concat $ fmap (\x -> x ++ lt) lv 


elemsPerVert = 11

renderables2CVD :: Renderables -> CombinedVertexData
renderables2CVD =
  foldl mkVs emptyCVD

  where

    calcNormal :: [[GLfloat]] -> [GLfloat] 
    calcNormal [ v0 , v1 , v2 ] =
        let [ uX , uY , uZ ] = zipWith (-) v1 v0
            [ vX , vY , vZ ] = zipWith (-) v2 v0
        in [ uY * vZ - uZ * vY , uZ * vX - uX * vZ , uX * vY - uY * vX ]
    calcNormal [ v0 , v1 ] = [0.0 , 0.0 , 0.0 ] 
    calcNormal _ = [0.0 , 0.0 , 0.0 ] 
    
    mkVs x (D.Triangle pts , shade) =
      let pos = fmap trpl2arr $ trpl2arr pts          
      in x { triangleVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral (shadeMode shade) , fromIntegral ( shadeModeVFG shade) ]))
                ++ triangleVs x }

    mkVs x (D.Line pts , shade) =
      let pos = fmap trpl2arr $ tpl2arr pts          
      in x { lineVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral (shadeMode shade) , fromIntegral ( shadeModeVFG shade)  ]))
                 ++ lineVs x }

    mkVs x (D.Point pt , shade) =
      let pos = fmap trpl2arr [pt]          
      in x { pointVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral ( shadeModeVFG shade)  , fromIntegral (shadeModeVFG shade) ]))
                 ++ pointVs x}  
      
    
type Descriptors = (Descriptor,Descriptor,Descriptor) 

initResources :: Renderables -> IO [Descriptor]
initResources rs =
  do de0 <- initTrianglesResources Points (pointVs (renderables2CVD rs))
     de1 <- initTrianglesResources Lines (lineVs (renderables2CVD rs))
     de2 <- initTrianglesResources Triangles (triangleVs (renderables2CVD rs))
     -- putStr (show de2)
     return $ reverse [de0 , de1 , de2]
     
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initTrianglesResources ::  PrimitiveMode -> [GLfloat] -> IO Descriptor
initTrianglesResources pm vertices = 

  do triangles <- genObjectName
     bindVertexArrayObject $= Just triangles

     -- putStr (show $ length vertices)
     -- let vertices = [

     let numVertices = (length vertices) 

         -- maskAttr = concat $ replicate numVertices mask0

     arrayBuffer <- genObjectName
     bindBuffer ArrayBuffer $= Just arrayBuffer
     withArray vertices $ \ptr -> do
       let size = fromIntegral (numVertices * sizeOf (head vertices))
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

     program <- loadShaders [
        ShaderInfo VertexShader (FileSource "data/shaders/shader3d.vert"),
        ShaderInfo FragmentShader (FileSource "data/shaders/shader3d.frag")]
     currentProgram $= Just program

     let firstIndex = 0
         vPosition = AttribLocation 0


     let ofst = (2 * 3 * 4 + 1 * 4 * 4 + 2 * 1 * 4)
     
     vertexAttribPointer vPosition $=
       (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset firstIndex))
     vertexAttribArray vPosition $= Enabled

     normalBuffer <- genObjectName
     bindBuffer ArrayBuffer $= Just normalBuffer
     withArray vertices $ \ptr -> do
       let size = fromIntegral (numVertices * sizeOf (head vertices))
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

     let normalPosition = AttribLocation 1

     vertexAttribPointer normalPosition $=
       (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 1 )))
     vertexAttribArray normalPosition $= Enabled


     colorBuffer <- genObjectName
     bindBuffer ArrayBuffer $= Just colorBuffer
     withArray vertices $ \ptr -> do
       let size = fromIntegral (numVertices * sizeOf (head vertices))
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

     let colorPosition = AttribLocation 2
 
     vertexAttribPointer colorPosition $=
       (ToFloat, VertexArrayDescriptor 4 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2)))
     vertexAttribArray colorPosition $= Enabled


     modeBuffer <- genObjectName
     bindBuffer ArrayBuffer $= Just modeBuffer
     withArray vertices $ \ptr -> do
       let size = fromIntegral (numVertices * sizeOf (head vertices))
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

     let modePosition = AttribLocation 3
 
     vertexAttribPointer modePosition $=
       (ToFloat, VertexArrayDescriptor 1 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2 + 4 * 4 * 1)))
     vertexAttribArray modePosition $= Enabled



     visFlagBuffer <- genObjectName
     bindBuffer ArrayBuffer $= Just visFlagBuffer
     withArray vertices $ \ptr -> do
       let size = fromIntegral (numVertices * sizeOf (head vertices))
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

     let visFlagPosition = AttribLocation 4
 
     vertexAttribPointer visFlagPosition $=
       (ToFloat, VertexArrayDescriptor 1 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2 + 4 * 4 * 1 +  1 * 4 * 1)))
     vertexAttribArray visFlagPosition $= Enabled


     return $ Descriptor pm triangles firstIndex (fromIntegral (div (length vertices) elemsPerVert) ) defaultLineWidth






data Viewport = Viewport
   { vpAlpha :: Float
   , vpBeta :: Float
   , vpGamma :: Float
   , vpScale :: Float
   , vpScreenDelta :: (Float , Float)
   }
  deriving Show

onDisplay :: Window -> Int -> Int -> VizGroupFlag -> Viewport -> Descriptor -> IO ()
onDisplay win w h vgf vp ds = do

  -- GL.clear [GL.DepthBuffer]
  blend $= Disabled
  vertexProgramPointSize $= Enabled
  clearDepth $= 1
  depthFunc $= Just Lequal
  cullFace $= Nothing
  lineWidth $= 2
  now <- GLFW.getTime
  -- blendFunc $= (SrcAlpha , OneMinusSrcAlpha)
  polygonSmooth $= Disabled

  
  let vMat = Vector4 (vpAlpha vp * 360) (vpBeta vp * 360) (vpGamma vp * 360) (vpScale vp)  --75.0 0.0 $ 1.0 * (-35.0 + 1.0 * 50.0 * sin (0.7 * (realToFrac $ fromJust now)))

  
  uniform (UniformLocation 0 ) $= (vMat :: Vector4 GLfloat)
  uniform (UniformLocation 1 ) $= (Vector2 (fromIntegral w) (fromIntegral h) :: Vector2 GLfloat)
  case now of
    Just nowD -> uniform (UniformLocation 2 ) $= nowD
    Nothing -> return ()


  let (Descriptor pm verts firstIndex numVertices _) = ds
  case pm of
    Lines -> uniform (UniformLocation 3 ) $= (0 :: GLfloat)
    Points -> uniform (UniformLocation 3 ) $= (0 :: GLfloat)
    _ -> uniform (UniformLocation 3 ) $= (1 :: GLfloat)

  uniform (UniformLocation 4 ) $= (1.0 :: GLfloat)
  uniform (UniformLocation 5 ) $= ((uncurry Vector2 (vpScreenDelta vp))  :: Vector2 GLfloat)
  uniform (UniformLocation 6 ) $= ((fromIntegral vgf) :: GLfloat)
    
  bindVertexArrayObject $= Just verts
  drawArrays pm firstIndex numVertices


onDisplayAll :: Window -> Int -> Int -> VizGroupFlag -> Viewport -> [Descriptor] -> IO ()
onDisplayAll win w h vfg vp = void . sequence . map (onDisplay win w h vfg vp) 

mouseToModel1d :: Int -> Int -> (Double , Double) -> (Float)
mouseToModel1d w h (x , y) =
  let p = 
            if w > h
            then (((x - (fromIntegral (w - h) * 0.5)) / fromIntegral h))
            else ((x / fromIntegral w))
  in realToFrac p


mouseToModel2d :: Int -> Int -> (Double , Double) -> (Float , Float)
mouseToModel2d w h (x , y) =
  let p = 
            if w > h
            then (((x - (fromIntegral (w - h) * 0.5)) / fromIntegral h)  , (y / fromIntegral h)  )
            else ((x / fromIntegral w) , (y - (fromIntegral (h - w) * 0.5)) / fromIntegral w)
  in second (\y -> 1.0 - y) $ (bimap realToFrac realToFrac) $ p

  -- let (Descriptor pm1 verts1 firstIndex1 numVertices1) = ds1  
  -- bindVertexArrayObject $= Just verts1
  -- drawArrays pm1 firstIndex1 numVertices1

  -- let (Descriptor pm2 verts2 firstIndex2 numVertices2) = ds2  
  -- bindVertexArrayObject $= Just verts2

  -- drawArrays pm2 firstIndex2 numVertices2


-- testRen :: IO ()
-- testRen =
--   do putStr "drawing test"
--      render ex1ren


-- testSf :: IO ()
-- testSf =
--   do putStr "drawing test"
--      render (toRenderables ex2drw)

-- testForce :: IO ()
-- testForce =
--   do putStr "drawing test force"
--      render (toRenderablesForce ex2drw)


-- testHycu :: IO ()
-- testHycu =
--   do putStr "drawing test force"
--      render (toRenderables (D.scale 0.5 $ (unitHyCubeSkel 3 2)))


-- test = testHycu

data RenderMode = Raw

-- renderAs :: (Colorlike a) => Drawing.GL.RenderMode -> Drawing a -> IO () 
-- renderAs Raw dw = render (toRenderablesForce dw)
  
  



