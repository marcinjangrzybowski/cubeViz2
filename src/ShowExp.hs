{-# LANGUAGE FlexibleContexts #-}
module ShowExp where


import Control.Monad             (unless, when, void)
import Control.Monad.Reader      (ReaderT , ask)
import Control.Monad.Writer      (tell)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Drawing.Base
import Drawing.GL
import Drawing.Example
import Data.List

import Data.Tuple.Extra

import Data.Maybe

import Syntax
import InteractiveParse

import Data.Bifunctor

import DrawExpr

import DataExtra

import Combi

import qualified UI.UI as UI


-- termDrawing :: SessionState -> IO ()
termDrawing dm ss@(SessionState ee _ _ e) =
   
      let drawing = drawExpr dm $ ssEnvExpr $ ss
          dim = getDim ee
      
      in bimap
            id
            (case dim of
               2 -> (embed 1 (const 0))
               3 -> (id)
            )
            drawing

data Msg =
    SetDescriptor Descriptors
  | LoadFile String
  | CycleDrawMode

data AppState = AppState
   { fileName        :: Maybe String
   , asSession       :: Maybe SessionState
   , asDrawMode      :: DrawExprMode
   , asViewport      :: Viewport 
   , asDragStartVP   :: Maybe Viewport
   }



main :: IO ()
main =
    UI.main $ UI.UIDescription
       { UI.uiDescInit                   = init 
       , UI.uiDescRenderFrame            = render
       , UI.uiDescUpdate                 = update
       , UI.uiListeners                  = processEvent
       }

    where

      render win w h descriptors =
         do
            viewport <- UI.getsAppState asViewport
            liftIO $ onDisplay win w h viewport descriptors 
        
      
      init =
        do args <- liftIO getArgs
           let fName = head args
           
           -- dsc <- liftIO $ initResources $ toRenderablesForce ex1drw
           tell $ pure (LoadFile fName)

           -- testB <- liftIO getLine
           -- liftIO $ putStr $ show testB
             
           return $ AppState
            { fileName        = Nothing
            , asViewport      = Viewport { vpAlpha = 1/5 , vpBeta = 0 , vpGamma = -0.1 }
            , asDrawMode      = Stripes --Scaffold --head drawExprModes
            , asDragStartVP   = Nothing
            , asSession       = Nothing
            }

      updateView =
        do (session , dm) <- UI.getsAppState (asSession &&& asDrawMode)
           case session of
             Nothing -> error "unhandled"
             Just session ->
                 case termDrawing dm session of
                      Left errMsg -> liftIO $ putStrLn errMsg
                      Right drawing ->
                        do
                           desc <- liftIO $ initResources $ toRenderablesForce drawing
                           UI.sendMsg $ SetDescriptor desc
      
      update msg =
        case msg of
          SetDescriptor dsc -> 
            do modify $ \s -> s
                  { UI.stateGLDesc  = Just dsc 
                  }
          LoadFile fName ->
            do UI.modifyAppState $ \s -> s
                  { fileName = Just fName 
                  }

               fileLoadingResult <-
                    liftIO $
                      do
                         handle <- (openFile fName ReadMode)
                         contents <- hGetContents handle
                         return $ parseInteractive contents
                        
               case fileLoadingResult of
                  Left errMsg -> liftIO $ putStrLn errMsg
                  Right sessionState -> do
                         UI.modifyAppState $ (\s -> s
                             { asSession = Just sessionState 
                              })
                         updateView
          CycleDrawMode -> do
               UI.modifyAppState $ \s -> s
                  { asDrawMode = rotateFrom (asDrawMode s) drawExprModes 
                  }                
               updateView

      processEvent ev =
            case ev of
              (UI.EventCursorPos _ x y) -> do
                let x' = round x :: Int
                    y' = round y :: Int
                -- printEvent "cursor pos" [show x', show y']
                mouseD <- gets UI.stateMouseDown
                x0 <- gets UI.stateDragStartX
                y0 <- gets UI.stateDragStartY
                
                when mouseD
                  (do s <- UI.getsAppState id 
                      case asDragStartVP s of
                         Nothing -> return ()
                         Just vp0 -> do
                           let newV  = (asViewport s)
                                   { vpAlpha = vpAlpha vp0 - ((realToFrac (y - y0)) / 2000.0)
                                   , vpGamma = vpGamma vp0 + ((realToFrac (x - x0)) / 2000.0) }
                           -- liftIO $ putStrLn $ show newV 
                           UI.setAppState $ s { asViewport = newV } 
                  )
              (UI.EventMouseButton _ mb mbs mk) -> do
                 when (mb == GLFW.MouseButton'1) $ do
                     let pressed = mbs == GLFW.MouseButtonState'Pressed
                     when pressed $
                       do UI.modifyAppState $ \s -> s
                            { asDragStartVP = Just (asViewport s)
                            }
                          
                     unless pressed $
                       UI.modifyAppState $ \s -> s
                         { asDragStartVP = Nothing
                         }
              (UI.EventKey win k scancode ks mk) -> do
                    when (ks == GLFW.KeyState'Pressed) $ do
                        -- Q, Esc: exit
                        when (k == GLFW.Key'C) $
                          UI.sendMsg $ LoadFile "data/input-to-viz/expr3d1-fill"
                        when (k == GLFW.Key'D) $ do
                          UI.sendMsg $ CycleDrawMode
                          
                          -- modify $ \s -> s { stateLogEvents = not ( stateLogEvents s) }           
              _ -> return ()
              
              
  -- let (init , render) = renderTask $ toRenderablesForce ex1drw

  -- in  

-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d




