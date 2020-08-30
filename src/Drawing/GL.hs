module Drawing.GL where

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

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices



data CombinedVertexData =
  CombinedVertexData { pointVs :: [GLfloat] , lineVs :: [GLfloat] , triangleVs :: [GLfloat] }

emptyCVD = CombinedVertexData { pointVs = [] , lineVs = [] , triangleVs = [] }




perVert :: [[a]] -> [a] -> [a]
perVert lv lt = concat $ fmap (\x -> x ++ lt) lv 

renderables2CVD :: Renderables -> CombinedVertexData
renderables2CVD =
  foldl mkVs emptyCVD

  where

    calcNormal :: [[GLfloat]] -> [GLfloat] 
    calcNormal [ v0 , v1 , v2 ] =
        let [ uX , uY , uZ ] = zipWith (-) v1 v0
            [ vX , vY , vZ ] = zipWith (-) v2 v0
        in [ uY * vZ - uZ * vY , uZ * vX - uX * vZ , uX * vY - uY * vX ]
    calcNormal [ v0 , v1 ] = calcNormal [ v0 , v1 , [0 , 0 , 0] ] 
    calcNormal _ = [1.0 , 0.0 , 0.0 ] 
    
    mkVs x (D.Triangle pts , cl) =
      let pos = fmap trpl2arr $ trpl2arr pts          
      in x { triangleVs = (perVert pos (calcNormal pos ++ color2arr cl)) ++ triangleVs x}

    mkVs x (D.Line pts , cl) =
      let pos = fmap trpl2arr $ tpl2arr pts          
      in x { lineVs = (perVert pos (calcNormal pos ++ color2arr cl)) ++ lineVs x}

    mkVs x (D.Point pt , cl) =
      let pos = fmap trpl2arr [pt]          
      in x { pointVs = (perVert pos (calcNormal pos ++ color2arr cl)) ++ pointVs x}  
      
    


initResources :: Renderables -> IO (Descriptor,Descriptor,Descriptor)
initResources rs =
  do de0 <- initTrianglesResources (pointVs (renderables2CVD rs))
     de1 <- initTrianglesResources (lineVs (renderables2CVD rs))
     de2 <- initTrianglesResources (triangleVs (renderables2CVD rs))
     return (de0 , de1 , de2)
     
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initTrianglesResources :: [GLfloat] -> IO Descriptor
initTrianglesResources vertices = 

  do triangles <- genObjectName
     bindVertexArrayObject $= Just triangles

     putStr (show $ length vertices)
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


     let ofst = (2 * 3 * 4 + 1 * 4 * 4 )

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


     return $ Descriptor triangles firstIndex (fromIntegral numVertices)



resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      uniform (UniformLocation 1 ) $= (Vector2 (fromIntegral w) (fromIntegral h) :: Vector2 GLfloat) 

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed win GLFW.Key'Right _ GLFW.KeyState'Pressed _ = putStr "xx"
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()


onDisplay :: Window -> (Descriptor , Descriptor , Descriptor) -> IO ()
onDisplay win dd@(ds0 , ds1 , ds2) = do
  GL.clearColor $= Color4 1 1 1 1
  GLFW.swapInterval 0
  GL.clear [ColorBuffer , DepthBuffer]

  blend $= Disabled
  vertexProgramPointSize $= Enabled
  clearDepth $= 1
  depthFunc $= Just Lequal
  -- cullFace $= Nothing
  lineWidth $= 2
  now <- GLFW.getTime
  -- blendFunc $= (SrcAlpha , OneMinusSrcAlpha)
  polygonSmooth $= Enabled
  let vMat =  Vector3 75.0 0.0 (-35.0 + 1.0 * 110.0 * sin (0.7 * (realToFrac $ fromJust now)))
  uniform (UniformLocation 0 ) $= (vMat :: Vector3 GLfloat)


  let (Descriptor verts0 firstIndex0 numVertices0) = ds0  
  bindVertexArrayObject $= Just verts0
  drawArrays Points firstIndex0 numVertices0

  let (Descriptor verts1 firstIndex1 numVertices1) = ds1  
  bindVertexArrayObject $= Just verts1
  drawArrays Lines firstIndex1 numVertices1

  let (Descriptor verts2 firstIndex2 numVertices2) = ds2  
  bindVertexArrayObject $= Just verts2
  drawArrays Triangles firstIndex2 numVertices2
  
  GLFW.swapBuffers win
  
  forever $ do
     GLFW.pollEvents
     onDisplay win dd


render :: Renderables -> IO ()
render rs =
    do
     GLFW.init
     GLFW.defaultWindowHints
     Just win <- GLFW.createWindow 640 480 "CubeViz2" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     GLFW.setWindowSizeCallback win (Just resizeWindow)
     GLFW.setKeyCallback win (Just keyPressed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     descriptors <- initResources rs
     onDisplay win descriptors
     GLFW.destroyWindow win
     GLFW.terminate



test :: IO ()
test =
  do putStr "drawing test"
     render ex1ren
