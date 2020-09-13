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

import Drawing.Color
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

import Data.Either

import DataExtra

import Combi

import qualified UI.UI as UI

import Abstract

asExpression = fmap ssEnvExpr . asSession

termDrawing :: UIApp (Maybe (Either String [Drawing ColorType]))
termDrawing =
   do appState <- UI.getAppState
      let drawings = fmap (drawExpr appState (asDrawMode appState)) $ (asExpression appState)
      return drawings
--           dim = getDim ee
      
--       in bimap
--             id
--             (case dim of
--                2 -> map (embed 1 (const 0))
--                3 -> map (id)
--             )
--             drawings

data Msg =
    SetDescriptor [Descriptor]
  | LoadFile String
  | CycleDrawMode

data AppState = AppState
   { fileName          :: Maybe String
   , asSession         :: Maybe SessionState
   , asDrawMode        :: DrawExprMode
   , asViewport        :: Viewport 
   , asDragStartVP     :: Maybe Viewport
   , asCursorAddress   :: Maybe Address
   }



data DrawExprMode = Stripes | StripesNoFill | Scaffold | Scaffold2
  deriving (Show , Eq)

drawExprModes = [
  Stripes , StripesNoFill ,
  Scaffold , Scaffold2  ]

drawExpr :: AppState -> DrawExprMode -> ((Env , Context) , Expr)
                  -> Either String ([Drawing (([String] , ExtrudeMode) , Color)])
drawExpr _ Stripes = fmap pure . mkDrawExprFill DefaultPT
drawExpr _ StripesNoFill = fmap pure . mkDrawExpr DefaultPT
drawExpr _ Scaffold = \e ->
   sequence $  [
                mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True })
              , mkDrawExprFill (ScaffoldPT { sptDrawFillSkelet = False })              
              ] <*> pure e
   
drawExpr as Scaffold2 = \e ->
   sequence $ (

              [ mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True }) ]
               ++
               maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
              ) <*> pure e 

type UIApp = UI.UI AppState [Descriptor] Msg

printExpr :: UIApp ()
printExpr =
  do mbExpr <- UI.getsAppState asExpression
     case mbExpr of
       Just (( _ , ctx) , expr) -> liftIO $ (putStrLn $ fromRight "no expresion to print" (toCode ctx expr))
       _ -> return ()

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
            liftIO $ onDisplayAll win w h viewport descriptors 
        
      
      init =
        do args <- liftIO getArgs
           let fName = head args
           
           -- dsc <- liftIO $ initResources $ toRenderablesForce ex1drw
           tell $ pure (LoadFile fName)

           -- testB <- liftIO getLine
           -- liftIO $ putStr $ show testB
             
           return $ AppState
            { fileName          = Nothing
            , asViewport        = Viewport { vpAlpha = 1/5 , vpBeta = 0 , vpGamma = -0.1 }
            , asDrawMode        = Scaffold2 -- Stripes --head drawExprModes
            , asDragStartVP     = Nothing
            , asSession         = Nothing
            , asCursorAddress   = Just ((enumerate 3 2 ) : (enumerate 3 1 )  : [])
            }

      updateView = 
                do tdRes <- termDrawing
                   case tdRes of
                        Nothing -> UI.sendMsg $ SetDescriptor []
                        Just (Left errMsg) -> liftIO $ putStrLn errMsg
                        Just (Right drawings) ->
                          do
                             desc <- liftIO $ initResources $ concat (map toRenderablesForce drawings)
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
                             { asSession =
                                let (ee , expr) = (mkExprGrid 3 2)
                                in Just $ SessionState ee [] (BType 0) expr
                               -- Just sessionState 
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
                        when (k == GLFW.Key'P) $ 
                          printExpr
                        when (k == GLFW.Key'Down) $ do
                          mbAddr <- UI.getsAppState asCursorAddress
                          case mbAddr of
                            Nothing -> return ()
                            Just addr -> do
                               UI.modifyAppState (\s -> s { asCursorAddress = Just (tailAlways addr) })
                               updateView
                            
                          -- modify $ \s -> s { stateLogEvents = not ( stateLogEvents s) }           
              _ -> return ()
              
              
  -- let (init , render) = renderTask $ toRenderablesForce ex1drw

  -- in  

-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d




