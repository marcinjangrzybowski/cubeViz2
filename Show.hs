import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import LoadShaders
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

mask0 = [
        Vertex2 (-1.0) (-0.05),  -- Triangle 1
        -- Vertex2   0.1 0.0 ,
        Vertex2   1.0  ( 0.05),
        Vertex2 (0.05)   1.0 ]

mask1 = [
        Vertex2 (1.0) (1.0),  -- Triangle 1
        -- Vertex2   0.1 0.0 ,
        Vertex2   1.0  ( 0.05),
        Vertex2 (0.05)   1.0 ]

brownish = [Vertex2 0.2 0.2 , Vertex2 0.9 1.0];

initResources :: IO Descriptor
initResources = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        Vertex2 (-0.90) (-0.90)] ++ mask0  ++ brownish  -- Triangle 1
        ++ [Vertex2   0.85  (-0.90)] ++  mask0 ++ brownish
        ++ [Vertex2 (-0.90)   0.85 ] ++  mask0  ++ brownish
        ++ [Vertex2   0.90  (-0.85)] ++  mask0  ++ brownish -- Triangle 2
        ++ [Vertex2   0.90    0.90 ] ++  mask0  ++ brownish
        ++ [Vertex2 (-0.85)   0.90 ] ++ mask0 ++ brownish :: [Vertex2 GLfloat]
      numVertices = (length vertices)

      -- maskAttr = concat $ replicate numVertices mask0
  
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "shader.vert"),
     ShaderInfo FragmentShader (FileSource "shader.frag")]
  currentProgram $= Just program

  let firstIndex = 0
      vPosition = AttribLocation 0

  -- m0AttrLoc <- get $ attribLocation program "M0"
  -- m1AttrLoc <- get $ attribLocation program "M1"
  -- putStrLn (show m0AttrLoc)  
  -- putStrLn (show m1AttrLoc)
  
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float (4 * 2 * 4 + 4 * 4 ) (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled


  m0Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m0Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m0Position = AttribLocation 1

  -- m0AttrLoc <- get $ attribLocation program "M0"
  -- m1AttrLoc <- get $ attribLocation program "M1"
  -- putStrLn (show m0AttrLoc)  
  -- putStrLn (show m1AttrLoc)
  
  vertexAttribPointer m0Position $=
    (ToFloat, VertexArrayDescriptor 2 Float (4 * 2 * 4 + 4 * 4 ) (bufferOffset (firstIndex + 2 * 4)))
  vertexAttribArray m0Position $= Enabled


  m1Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m1Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m1Position = AttribLocation 2

  -- m0AttrLoc <- get $ attribLocation program "M0"
  -- m1AttrLoc <- get $ attribLocation program "M1"
  -- putStrLn (show m0AttrLoc)  
  -- putStrLn (show m1AttrLoc)
  
  vertexAttribPointer m1Position $=
    (ToFloat, VertexArrayDescriptor 2 Float (4 * 2 * 4 + 4 * 4 ) (bufferOffset (firstIndex + 2 * 4 * 2)))
  vertexAttribArray m1Position $= Enabled


  m2Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m2Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m2Position = AttribLocation 3

  -- m0AttrLoc <- get $ attribLocation program "M0"
  -- m1AttrLoc <- get $ attribLocation program "M1"
  -- putStrLn (show m0AttrLoc)  
  -- putStrLn (show m1AttrLoc)
  
  vertexAttribPointer m2Position $=
    (ToFloat, VertexArrayDescriptor 2 Float (4 * 2 * 4 + 4 * 4 ) (bufferOffset (firstIndex + 2 * 4 * 3)))
  vertexAttribArray m2Position $= Enabled

  colorBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just colorBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let colorPosition = AttribLocation 4

  -- m0AttrLoc <- get $ attribLocation program "M0"
  -- m1AttrLoc <- get $ attribLocation program "M1"
  -- putStrLn (show m0AttrLoc)  
  -- putStrLn (show m1AttrLoc)
  
  vertexAttribPointer colorPosition $=
    (ToFloat, VertexArrayDescriptor 4 Float (4 * 2 * 4 + 4 * 4 ) (bufferOffset (firstIndex + 2 * 4 * 4)))
  vertexAttribArray colorPosition $= Enabled



  return $ Descriptor triangles firstIndex (fromIntegral numVertices)


-- resizeWindow :: GLFW.WindowSizeCallback
-- resizeWindow win w h =
--     do
--       GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
--       GL.matrixMode $= GL.Projection
--       GL.loadIdentity
--       GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()


main :: IO ()
main = do
   GLFW.init
   GLFW.defaultWindowHints
   Just win <- GLFW.createWindow 640 480 "Haskel OpenGL Tutorial 02" Nothing Nothing
   GLFW.makeContextCurrent (Just win)
   -- GLFW.setWindowSizeCallback win (Just resizeWindow)
   GLFW.setKeyCallback win (Just keyPressed)
   GLFW.setWindowCloseCallback win (Just shutdown)
   descriptor <- initResources
   onDisplay win descriptor
   GLFW.destroyWindow win
   GLFW.terminate


onDisplay :: Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 1 1 1 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  blend $= Enabled
  blendFunc $= (SrcAlpha , OneMinusSrcAlpha)
  polygonSmooth $= Disabled
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win
  
  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor


-- import Graphics.Rendering.OpenGL
-- import Graphics.UI.GLUT

-- import Drawing.Base



-- s1 :: DrawingGL
-- s1 = Drawing [ (unitHyCube 3 , SShape ([Rgba 0 1 0 1])) ]





-- -- zz = Window

-- main = do
--   getArgsAndInitialize
--   --currentWindow $= Just zz
-- --  createSubWindow zz (Position 10 10) (Size 200 200)
--   createWindow "Triangle"
--   displayCallback $= display  
--   matrixMode $= Projection
--   loadIdentity
--   ortho2D (- 0.1) 1.1 (- 0.1) 1.1
--   matrixMode $= Modelview 0
--   blendFunc $= (One, One) 
--   blend $= Enabled
--   mainLoop


-- -- triangleL :: [GLfloat] -> IO ()
-- triangleL =
--   sequence . map triangle
--      -- return ()

-- red = (0.7 , 0 , 0)

-- green = (0 , 0.7 , 0)

-- blue = (0 , 0 , 0.7)


-- mask1 = ((0.3,0.4),(0.8,0.6),(0.2,-0.05))
  
-- display = do
--   clear [ColorBuffer]
--   let lOfTriangles =
--          [
--            (blue , mask1)
--          , (red , ((0,0),(0,1),(1,0)))
--          , (green , ((1,1),(0,1),(1,0)))

--          ]
--   renderPrimitive Triangles $ (sequence (map triangle lOfTriangles)) 
--   swapBuffers



-- triangle :: ((GLfloat , GLfloat , GLfloat) , ((GLfloat , GLfloat) ,  (GLfloat , GLfloat) , (GLfloat , GLfloat))) -> IO ()
-- triangle ((r , g , b) , (c0 , c1 , c2)) =
--   do color  (Color3  r g b)
--      let toV = \(x , y) -> vertex $ Vertex2 x y
--      toV c0
--      toV c1
--      toV c2
     
--      -- corner 0 1 0 (20 + delta) (5 + delta)
--      -- corner 0 0 1 (5 + delta) (25 + delta)
  
-- -- corner r g b x y = do color  (Color3  r g b)
-- --                       vertex (Vertex2 x y)
-- --27263183





