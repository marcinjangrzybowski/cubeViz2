{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ShowExp where


import Control.Monad             (unless, when, void)
import Control.Monad.Reader      (ReaderT , ask , reader)
import Control.Monad.Writer      (WriterT , tell , mapWriterT , listen)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put , mapRWST)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment
import Data.Time.Clock

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Drawing.Color
import Drawing.Base
import Drawing.GL
import Drawing.Example
import Data.List

import qualified System.Console.Pretty as SCP
import qualified System.Console.ANSI as SCA

import Data.Tuple.Extra


import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Maybe
import Data.Function

import Syntax
import InteractiveParse

import qualified Data.Bifunctor as Bf

import DrawExpr

import Data.Either

import DataExtra

import Combi

import qualified UI.UI as UI

import Abstract
import ConsolePrint

import ExprTransform

import Debug.Trace

import ExprParser (parseExpr)

data Msg =
    LoadFile String
  | LoadGrid
  | TermInput
 deriving (Show)


data DisplayPreferences = DisplayPreferences
   { dpShowFilling       :: Bool
   }

defaultDisplayPreferences = DisplayPreferences
   { dpShowFilling        = True
   }
   
data AppState = AppState
   { fileName             :: Maybe String
   , asSession            :: SessionState
   , asCub                :: Cub () (Either Int CellExpr)
   , asDrawMode           :: DrawExprMode
   , asViewport           :: Viewport 
   , asDragStartVP        :: Maybe Viewport
   , asUserMode           :: UserMode
   , asKeyPressed         :: Bool
   , asMessage            :: String
   , asDisplayPreferences :: DisplayPreferences
   , asTime               :: DiffTime
   }

asCursorAddress :: AppState -> Maybe Address
asCursorAddress as = 
  case (asUserMode as) of
    UMNavigation x -> Just x
    UMSelectGrid {} -> Just $ umEditedCell (asUserMode as)
    UMEditTail {} -> Just $ umEditedCell (asUserMode as)
    UMAddSubFace {} -> Just $ fst $ umSubFaceToAdd (asUserMode as)

asSubFaceToAdd :: AppState -> Maybe (Address , SubFace)
asSubFaceToAdd as =
    case (asUserMode as) of
      UMAddSubFace x -> Just x
      _ -> Nothing

asExpression = ssEnvExpr . asSession

-- asCub :: AppState -> (Cub () (Either Int CellExpr))
-- asCub = toCub . asExpression

asViewportProc :: AppState -> Viewport
asViewportProc appS =
  case getDim $ asCub appS of
    2 -> Viewport 0 0 0
    _ -> asViewport appS

    
data UserMode =
    Idle
  | UMNavigation { umCoursorAddress :: Address }
  | UMAddSubFace { umSubFaceToAdd :: (Address , SubFace) }
  | UMEditCell { umEditedCell :: Address }
  | UMEditTail { umEditedCell :: Address , umTailPosition :: Int }
  | UMSelectGrid { umEditedCell :: Address , umGSD :: GridSelectionDesc }


data DrawExprMode = Stripes | StripesNoFill | Scaffold | Scaffold2
  deriving (Show , Eq)

drawExprModes = [
  -- Stripes ,
  Scaffold  ]

drawExpr :: AppState -> DrawExprMode -> ((Env , Context) , Expr)
                  -> Either String ([Drawing (([String] , ExtrudeMode) , Color)])
-- drawExpr _ Stripes = fmap pure . mkDrawExprFill DefaultPT
-- drawExpr _ StripesNoFill = fmap pure . mkDrawExpr DefaultPT 
-- drawExpr _ Scaffold = \e ->
--    sequence $  [
--                 mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True , sptCursorAddress = Nothing })
--               , mkDrawExprFill (ScaffoldPT { sptDrawFillSkelet = False  , sptCursorAddress = Nothing })              
--               ] <*> pure e
   
drawExpr as Scaffold = \e ->

   let (sptCA , sptMSFC) =
          case (asSubFaceToAdd as) of
             Nothing -> (asCursorAddress as , Nothing)
             Just x -> (Nothing , Just x)
  
   in sequence $ (

              [ mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True
                                           , sptCursorAddress = sptCA
                                           , sptMissingSubFaceCursor = sptMSFC
                                           , sptScaffDim = 1})
              , mkDrawExprFill (DefaultPT { dptCursorAddress = sptCA
                                          , dptShowFill = dpShowFilling $ asDisplayPreferences as
                                          , dptFillFactor = 1.0
                                              -- 0.5 * (sin (realToFrac $ asTime as) + 1)

                                          })
              ]
               -- ++
               -- maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
              ) <*> pure e 

type UIApp = UI.UI AppState [Descriptor] Msg

type UIAppDesc  = UI.UIWithInfo AppState [Descriptor] Msg


main :: IO ()
main =
    UI.main $ UI.UIDescription
       { UI.uiDescInit                   = initialize 
       , UI.uiDescRenderFrame            = render
       , UI.uiDescUpdate                 = update
       , UI.uiListeners                  = processEvent
       , UI.uiUpdateGLDescriptor         = updateGL
       }

render win w h descriptors =
   do
      viewport <- UI.getsAppState asViewportProc
      liftIO $ onDisplayAll win w h viewport descriptors 
      currTime <- fmap utctDayTime $ liftIO $ getCurrentTime
      UI.modifyAppState (\s -> s { asTime = currTime })
      -- UI.flushDisplay
      return ()
      
initialize =
  do args <- liftIO getArgs
     let fName = head args

     mbInitialSessionState <- liftIO (loadFile fName)

     currTime <- fmap utctDayTime $ liftIO $ getCurrentTime
     
     case mbInitialSessionState of
       Right initialSessionState -> return $ Just $ 
             AppState
              { fileName          = Just fName
              , asViewport        = Viewport { vpAlpha = 1/5 , vpBeta = 0 , vpGamma = -0.1 }
              , asDrawMode        = Scaffold -- Stripes --   --head drawExprModes
              , asDragStartVP     = Nothing
              , asSession         = initialSessionState
              , asCub             = toCub $ ssEnvExpr initialSessionState
              , asUserMode        = UMNavigation []
              , asKeyPressed      = False
              , asMessage         = ""
              , asDisplayPreferences = defaultDisplayPreferences
              , asTime            = currTime
              }
       Left errorMsg -> do
          liftIO $ putStrLn errorMsg
          return Nothing
             

updateView :: UIApp ()     
updateView = return ()

update msg = return ()


inputTerm :: (Env , Context) ->  UIApp (Maybe CellExpr)
inputTerm (env , ctx) =
  do let dimsInCtx = unConstrainedDimsOnly ctx
     consolePrint $ "inputTerm: " ++ "dims in context : " ++ intercalate "," dimsInCtx
     
     exprR <- fmap (parseExpr ctx) $ liftIO (getLine)
     case exprR of
       Right expr -> do let ce = toCellExpr (env , ctx) expr
                        consolePrint $ toString (env , ctx) expr
                        return $ Just ce
       Left err -> do consolePrint $ show err
                      return Nothing    
     
     
setUserMode :: UserMode -> UIApp ()
setUserMode um = do
   UI.modifyAppState (\s ->
    s { asUserMode = um })
   UI.flushDisplay


data PEMode = PERuntime | PEInfo


processEvent :: UI.Event -> UIApp ()         
processEvent ev = do
  mapRWST (fmap (\(a , b , (_ , c) ) -> (a , b , c))) $ eventsGlobal PERuntime ev
  mode <- UI.getsAppState asUserMode
  mapRWST (fmap (\(a , b , (_ , c) ) -> (a , b , c))) $ modesEvents PERuntime mode ev
   


extractInfo = mapRWST (fmap (\((_ , (a , _)) , b , (_ , c) ) -> (a , b , c))) . listen


-- mkEventsInfo :: UI.Event -> UIApp String        
-- mkEventsInfo ev = do
--   globalInfo <- extractInfo $ eventsGlobal PEInfo ev
--   mode <- UI.getsAppState asUserMode
--   modalInfo <- extractInfo $ modesEvents PEInfo mode ev
--   return $ ("g: " ++ globalInfo ++ "\nm: " ++ modalInfo)  


eventsInfo :: UIApp String
eventsInfo =
  do
     globalInfo <- appDescHelp $ eventsGlobal PEInfo
     mode <- UI.getsAppState asUserMode
     modalInfo <- appDescHelp $  modesEvents PEInfo mode
     return $ (globalInfo  ++ "\n" ++ modalInfo)  

  where
    

    keHelp :: (UI.Event -> UIAppDesc ()) -> (GLFW.Key -> UIAppDesc ())
    keHelp f k = do 
        win <- reader UI.envWindow
        f (UI.EventKey win k 0 (GLFW.KeyState'Pressed)
                          (GLFW.ModifierKeys False False False False False False) ) 

    appDescHelp x =
        fmap (intercalate "\n")
      $ fmap (fmap (\(k , inf) -> "[" ++ k ++ "]: " ++ inf))
      $ fmap (fmap (first (drop 4 . show)) . (filter (not . null . snd)))
      $ UI.collect allKeys (extractInfo . (keHelp $ x) )
    
    infoStr :: (GLFW.Key , String) -> String
    infoStr = show
      
consolePrint :: String -> UIApp ()
consolePrint str = do
  UI.modifyAppState (\s ->
    s { asMessage = str })
  UI.flushDisplay


asTailAddress :: AppState -> Maybe Int
asTailAddress as =
  case (asUserMode as) of
    UMEditTail {} -> Just $ umTailPosition (asUserMode as) 
    _ -> Nothing 

printExpr :: UIApp ()
printExpr =
  do appS <- UI.getAppState
     cub <- UI.getsAppState asCub
     (ee , expr) <- UI.getsAppState asExpression
     addrs <- UI.getsAppState asCursorAddress

     liftIO
          $ do (putStrLn $ 
                 (printCub ee (CubPrintData {cpdCursorAddress = addrs , cpdTailAddress = asTailAddress appS}) [] cub))




modifyDrawingPreferences :: (DisplayPreferences -> DisplayPreferences) -> UIApp ()
modifyDrawingPreferences f = do
    UI.modifyAppState (\s ->
               let ss = (asDisplayPreferences s)
               in s {
                       asDisplayPreferences = 
                       f ss
                                })
    UI.flushDisplay

  

setFromCub :: Cub () (Either Int CellExpr) -> UIApp () 
setFromCub newCub = do
    UI.modifyAppState (\s ->
               let ss = (asSession s)
                   newS = ssSetExpression ss (fromCub (fst (ssEnvExpr ss) ) newCub)
               in s {
                       asSession = 
                       newS
                     , asCub = newCub -- toCub $ ssEnvExpr newS
                                })
    UI.flushDisplay

transformExpr :: CubTransformation (Either Int CellExpr) -> UIApp ()
transformExpr trns =
   do
      appS <- UI.getAppState
      case (applyTransform trns) (asCub appS) of
        (Left err) -> consolePrint $ "transform error: " ++ err
        (Right newCub) -> setFromCub newCub
        

printExprCode :: UIApp ()
printExprCode =
  do (ee , expr) <- UI.getsAppState asExpression
     liftIO $ do SCA.clearScreen
                 (putStrLn 
                    $ fromRight "error while printing expr" (toCode ee expr))

setAddSubFaceMode :: Address -> UIApp ()
setAddSubFaceMode addr = do
            appS <- UI.getAppState
            let cub = asCub appS
            let vsf = Set.toList $ vaccantSubFaces cub addr
            case vsf of
                 [] -> return ()
                 (x : _) -> setUserMode (UMAddSubFace (addr , x))

               -- Just _ -> do UI.modifyAppState $ \s -> s { asSubFaceToAdd = Nothing }
               --              updateView

setNavMode :: Address -> UIApp ()
setNavMode addr = setUserMode (UMNavigation addr)


liftPEM :: UIApp a -> UIAppDesc a
liftPEM = mapRWST (fmap (\(a , b , c ) -> (a , b , ("" , c))))
  

helperPEM :: PEMode -> String -> UIApp () ->  UIAppDesc ()
helperPEM PERuntime _ x = liftPEM x 
helperPEM PEInfo x _ = tell (x , []) 

eventsGlobal :: PEMode -> UI.Event -> UIAppDesc ()
eventsGlobal pem ev =
      let inf = helperPEM pem in
      case ev of
        (UI.EventCursorPos _ x y) -> inf "" $ do
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
        (UI.EventMouseButton _ mb mbs mk) -> inf "" $ do
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
                  when (k == GLFW.Key'Escape) $ inf "Quit" $
                    liftIO $ GLFW.setWindowShouldClose win True
                  -- ?: print instructions
                  -- when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
                  --   liftIO printInstructions
                  -- -- i: print GLFW information
                  -- when (k == GLFW.Key'I) $
                  --   liftIO $ putStrLn "x"
                  -- when (k == GLFW.Key'E) $
                    -- modify $ \s -> s { stateLogEvents = not ( stateLogEvents s) }
                  -- when (k == GLFW.Key'F) $
                    -- liftIO $ GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable False


                  -- when (k == GLFW.Key'O) $
                  --   UI.sendMsg $ LoadFile "data/input-to-viz/expr3d1-fill"
                  -- when (k == GLFW.Key'G) $
                  --   UI.sendMsg $ LoadGrid 
                  -- when (k == GLFW.Key'D) $ do
                  --   UI.sendMsg $ CycleDrawMode
                  when (k == GLFW.Key'P) $ inf "Print Expression" $
                    printExprCode
                  when (k == GLFW.Key'F) $ inf "Toggle Fillings" $
                    modifyDrawingPreferences
                      (\dp ->
                         dp { dpShowFilling = not $ dpShowFilling dp })
        _ -> return ()    





modesEvents :: PEMode -> UserMode -> UI.Event -> UIAppDesc ()         

modesEvents _ Idle ev = return ()

modesEvents pem um@(UMNavigation { umCoursorAddress = addr }) ev = do 
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
     handleCellManipulationEvents pem ev addr
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
             when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'H) $ inf "EditHead" $ do
                    initEditHead addr 

                  when (k == GLFW.Key'T) $ do
                    initEditTail pem addr

                    
                  when (k == GLFW.Key'A) $ do
                    case (uncons addr) of
                      Just (_ , addrTail) -> inf "Add sub-face" $ setAddSubFaceMode addrTail
                      Nothing -> return ()
                    
                    
                  when (k == GLFW.Key'C) $ do
                    if (GLFW.modifierKeysControl mk)
                    then inf "Delete sub-face with subfaces" $ transformExpr (RemoveCell addr)                            
                    else inf "Delete sub-face" $ transformExpr (RemoveCellLeaveFaces addr)
                    inf "" $ setNavMode (fullSF (getDim (head addr)) : (tailAlways addr))

                  when (k == GLFW.Key'S) $ inf "Split cell" $ do
                    transformExpr (SplitCell addr)
                    
                  when (k == GLFW.Key'Backspace) $ inf "Insert Hole" $ do
                    transformExpr (ReplaceAt addr (Cub undefined (Left 0)))
                    

                  when (isArrowKey k) $ inf "" $ do                                        
                    UI.modifyAppState (\s ->
                       let nav = fromJust (arrowKey2nav k)
                           jna = fromRight (addr) (cubNav (asCub appS) addr nav)  

                       in s { asUserMode = UMNavigation jna })
                    UI.flushDisplay

       
        _ -> return ()


modesEvents pem um@(UMEditCell addr  ) ev = undefined

modesEvents pem um@(UMEditTail addr pos ) ev = do 
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem

         localCub = fromJust $ cubPick addr $ cub

         tail =
            case localCub of
                  (Cub _ (Right (CellExpr vI tail))) -> tail
                       
                  _ -> error "??"
         
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
            when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
                     -- transformExpr (AddSubFace (addr , sf))
                     setUserMode (UMNavigation addr)

                  when (k == GLFW.Key'Q) $ inf "Abort" $ do
                    setUserMode $ UMNavigation addr

                  when (k == GLFW.Key'T) $ inf "Cycle Tail Positions" $ do
                    setUserMode $ (UMEditTail addr (mod (pos + 1) (length tail) ) )

                  -- when (isArrowKey k) $ inf "nav" $ do
                
                  --    let nav = fromJust (arrowKey2nav k)
                  --        vcnt = Set.toList $ vaccantSubFaces cub addr
                  --        sfNext =
                  --           case nav of
                  --              DNext -> rotateFrom sf vcnt
                  --              DPrev -> rotateFrom sf $ reverse vcnt
                  --              _ -> sf
                                         
                  --    setUserMode $ UMAddSubFace (addr , sfNext)
                
       
        _ -> return ()


modesEvents pem um@(UMAddSubFace (addr , sf) ) ev = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
            when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
                     transformExpr (AddSubFace (addr , sf))
                     setUserMode (UMNavigation { umCoursorAddress = sf : addr })

                  when (k == GLFW.Key'Q) $ inf "Abort" $ do
                    setUserMode $ UMNavigation addr


                  when (isArrowKey k) $ inf "nav" $ do
                
                     let nav = fromJust (arrowKey2nav k)
                         vcnt = Set.toList $ vaccantSubFaces cub addr
                         sfNext =
                            case nav of
                               DNext -> rotateFrom sf vcnt
                               DPrev -> rotateFrom sf $ reverse vcnt
                               _ -> sf
                                         
                     setUserMode $ UMAddSubFace (addr , sfNext)
                
       
        _ -> return ()

modesEvents pem um@(UMSelectGrid addr gsd ) ev = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
            when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
                    gsdRunFinalAction gsd
                    setUserMode (UMNavigation { umCoursorAddress = addr })

                  when (k == GLFW.Key'Q) $ inf "Abort" $ do
                    gsdRunAbortAction gsd
                    setUserMode $ UMNavigation addr


                  when (isArrowKey k) $ inf "nav" $ do
                
                     let nav = fromJust (arrowKey2nav k)
                     
                     gsdRunDirection gsd nav 
                
       
        _ -> return ()


rotationIndexes :: Address -> UIApp (Maybe (Int , Int))
rotationIndexes addr = do
  appS <- UI.getAppState
  pd <- pressedDims addr
  let n = getDim $ fromJust $ cubPick addr $ asCub appS
  return $ case n of
              2 -> Just (0 , 1)
              3 -> case (Set.toList pd) of
                        [ 0 ] -> Just (1,2)
                        [ 1 ] -> Just (0,2)
                        [ 2 ] -> Just (0,1)
                        _ -> Nothing
              _ -> Nothing
                 
pressedDims :: Address ->  UIApp (Set.Set Int)  
pressedDims addr = do
    appS <- UI.getAppState
    pd <- pressedDigits
    let n = getDim $ fromJust $ cubPick addr $ asCub appS
    let dimsL = (Set.map $ flip (-) 1) $ pd
    return $ Set.filter ((>) n) dimsL
       
  
handleCellManipulationEvents :: PEMode -> UI.Event -> Address -> UIAppDesc ()
handleCellManipulationEvents pem ev addr = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
         
      
     pd <- liftPEM $ pressedDims addr

     ri <- liftPEM $ rotationIndexes addr

     
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
             when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'I && length pd > 0) $ inf "negate dimensions" $ do
                      transformExpr (MapAt addr (Right . negateDimIndexes pd))
                  when (k == GLFW.Key'R && isJust ri) $ inf "rotate dimensions" $ do
                      transformExpr (MapAt addr (Right . rotateDimIndexes (fromJust ri)))

                  -- -- when (k == GLFW.Key'T) $ inf "Enter term" $ do 
                  -- --    let ((env , ctx) , _) = asExpression appS
                  -- --    mbTerm <- inputTerm (env , contextAt ctx addr cub)
                  -- --    let toPut = maybe (Left 0) Right mbTerm
                  -- --    transformExpr (ReplaceAt addr toPut)


                  -- when (k == GLFW.Key'A) $ do
                  --   case (uncons addr) of
                  --     Just (_ , addrTail) -> inf "Add sub-face" $ setAddSubFaceMode addrTail
                  --     Nothing -> return ()


                  -- when (k == GLFW.Key'C) $ do
                  --   if (GLFW.modifierKeysControl mk)
                  --   then inf "Delete sub-face with subfaces" $ transformExpr (RemoveCell addr)                            
                  --   else inf "Delete sub-face" $ transformExpr (RemoveCellLeaveFaces addr)
                  --   inf "" $ setNavMode (fullSF (getDim (head addr)) : (tailAlways addr))

                  -- when (k == GLFW.Key'S) $ inf "Split cell" $ do
                  --   transformExpr (SplitCell addr)

                  -- when (k == GLFW.Key'Backspace) $ inf "Insert Hole" $ do
                  --   transformExpr (ReplaceAt addr (Cub undefined (Left 0)))


                  -- when (isArrowKey k) $ inf "" $ do                                        
                  --   UI.modifyAppState (\s ->
                  --      let nav = fromJust (arrowKey2nav k)
                  --          jna = fromRight (addr) (cubNav (asCub appS) addr nav)  

                  --      in s { asUserMode = UMNavigation jna })
                  --   UI.flushDisplay


        _ -> return ()

modalConsoleView :: UserMode -> UIApp ()
modalConsoleView um@(UMSelectGrid addr gsd ) = do
  let cho = gridMapWithIndex (\ij -> if ij == gsdPosition gsd then SCP.bgColor SCP.Yellow else id ) (gsdChoices gsd)
      choStr = concat $ fmap ((++) "\n" . concat . fmap ((++) " ") ) $  cho
  liftIO $ putStrLn choStr
  

modalConsoleView _ = return ()
                 
drawingInterpreter :: DrawingInterpreter ColorType
drawingInterpreter =
  DrawingInterpreter [] 
   (\case
      2 -> Just (toRenderablesIgnore . embed 2 (const 0))
      3 -> Just toRenderablesForce
      

      
      _ -> Nothing)

updateGL :: UIApp [Descriptor]
updateGL = 
   do appState <- UI.getAppState
      let dRes = drawExpr appState (asDrawMode appState) $ (asExpression appState)
      printConsoleView
      case dRes of
        (Left errMsg) -> do
             liftIO $ putStrLn errMsg
             return []
        (Right drawings) -> do
             let tryToRen = toRenderableDI drawingInterpreter (concat drawings)
             case tryToRen of
               Left msg -> error msg
               Right rens ->

                 liftIO $ initResources $ snd rens

   where
     printConsoleView :: UIApp ()
     printConsoleView = do
        liftIO SCA.clearScreen
        ei <- eventsInfo
        liftIO $ putStrLn ei
        liftIO $ putStrLn ""
        printExpr
        -- liftIO $ putStrLn ""
        -- pKS <- fmap (\pK -> show (Set.toList pK)) UI.pressedKeys
        -- liftIO $ putStrLn pKS  
        uMsg <- UI.getsAppState asMessage
        UI.getsAppState asUserMode >>= modalConsoleView
        liftIO $ putStrLn uMsg


      
----

loadFile :: String -> IO (Either String SessionState) 
loadFile fName =   
   liftIO $ do          
       handle <- (openFile fName ReadMode)
       contents <- hGetContents handle
       return $ parseInteractive contents 


initEditTail :: PEMode -> Address -> UIAppDesc ()
initEditTail pem addr = do
    appS <- UI.getAppState
    let inf = helperPEM pem
        cub = fromJust $ cubPick addr $ asCub appS
    case cub of
      (Cub _ (Right (CellExpr vI (_ : _)))) ->
           inf "Edit Tail" $ setUserMode $ UMEditTail addr 0
      _ -> return ()

initEditHead :: Address -> UIApp ()
initEditHead addr = do
  appS <- UI.getAppState
  let (ee0@(env , ctx0@(Context vars _)) , _) = asExpression appS
      initialCub = fromJust $ cubPick addr (asCub appS)
      ctx = contextAt ctx0 addr (asCub appS)
      varsInCtx = binsBy (getCTyDim env ctx . snd . fst) $ zip (reverse vars) (fmap VarIndex [0..])
      varsInCtxGrid = (fmap snd $ Map.toList varsInCtx)
      
      (initPos , initTail) =
         case initialCub of
            Cub _ (Right (CellExpr vI tail)) ->
               (fromJust $ gridIndexOf vI $ fmap (fmap (snd)) varsInCtxGrid ,
                 fmap snd tail)
            _ -> ((0,0) , [])
      
      gsd = GridSelectionDesc
       { gsdChoices = fmap (fmap (fst . fst)) varsInCtxGrid 
       , gsdPosition = initPos
       , gsdAction =
           \_ nPos -> do
             let newVi = snd $ gridLookup nPos varsInCtxGrid 
             let newCell = mkCellExprForceDefTail (env , ctx) newVi initTail
             transformExpr (ReplaceAt addr (Cub undefined (Right newCell)))
       , gsdFinalAction = \_ _ -> return ()
       , gsdAbortAction = transformExpr (ReplaceAt addr initialCub)
       }
  gsdAction gsd (gridLookup initPos (gsdChoices gsd)) initPos
  setUserMode $ UMSelectGrid addr gsd
  


data GridSelectionDesc =
  GridSelectionDesc
  { gsdChoices :: [[String]]
  , gsdPosition :: (Int , Int)
  , gsdAction :: String -> (Int , Int) -> UIApp()
  , gsdFinalAction :: String -> (Int , Int) -> UIApp ()
  , gsdAbortAction :: UIApp ()
  }
      

gridLookup :: (Int , Int) -> [[a]] -> a
gridLookup (i , j) x = (x !! i) !! j

gridIndexOf :: (Eq a) => a -> [[a]] -> Maybe (Int , Int)
gridIndexOf a [] = Nothing 
gridIndexOf a ([] : xs) = fmap (first ((+) 1)) $ gridIndexOf a xs 
gridIndexOf a (x : xs) =
  case elemIndex a x of
    Nothing -> gridIndexOf a ([] : xs)
    Just j -> Just (0 , j)
  

gridMapWithIndex :: ((Int , Int) -> a -> a) -> [[a]] -> [[a]]
gridMapWithIndex f l = zipWith (\i -> zipWith (\j -> f (i , j) ) [0..]) [0..] l

gsdRunFinalAction :: GridSelectionDesc -> UIApp ()
gsdRunFinalAction gsd =
  gsdFinalAction gsd (gridLookup (gsdPosition gsd) (gsdChoices gsd)) (gsdPosition gsd)

gsdRunAbortAction :: GridSelectionDesc -> UIApp ()
gsdRunAbortAction gsd =
  gsdAbortAction gsd

-- gsdRunAction :: GridSelectionDesc -> AppState
-- gsdRunAction gsd =
--   gsdAction gsd (gridLookup (gsdPosition gsd) (gsdChoices gsd)) (gsdPosition gsd)

gsdRunDirection :: GridSelectionDesc -> Direction -> UIApp ()
gsdRunDirection gsd d =
  let newPos = mv d
      
       
  in do  
        gsdAction gsd (gridLookup newPos (gsdChoices gsd)) newPos                 
        UI.modifyAppState (\appS -> appS { asUserMode =
                                (\(UMSelectGrid addr gsd) ->
                                     UMSelectGrid addr
                                      (gsd { gsdPosition = newPos } ) )
                                 (asUserMode appS) } )
        UI.flushDisplay
          
  where

    (i , j) = gsdPosition gsd
    
    cho = gsdChoices gsd
    
    mv DNext = (i , mod (j + 1) (length (cho !! i))) 
    mv DPrev = (i , mod (j - 1) (length (cho !! i)))
    mv DParent = (mod (i - 1) (length cho) , mod (j) (length (cho !! (i - 1))))               
    mv DChild = (mod (i + 1) (length cho) , mod (j) (length (cho !! (i + 1))))              
                 
-- gsdTest :: AppState -> GridSelectionDesc
-- gsdTest appS =
--   GridSelectionDesc
--    [explode "abcde" , explode "marcin" , explode "grzybowski" ]
--    (1 , 2)
--    (\_ _ -> return ())
--    (\_ _ -> return ())
   
--
              
  -- let (init , render) = renderTask $ toRenderablesForce ex1drw

  -- in  

-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d


---- helpers

isArrowKey GLFW.Key'Down = True
isArrowKey GLFW.Key'Up = True
isArrowKey GLFW.Key'Left = True
isArrowKey GLFW.Key'Right = True
isArrowKey _ = False

isDigitKey GLFW.Key'1 = True
isDigitKey GLFW.Key'2 = True
isDigitKey GLFW.Key'3 = True
isDigitKey GLFW.Key'4 = True
isDigitKey GLFW.Key'5 = True
isDigitKey GLFW.Key'6 = True
isDigitKey GLFW.Key'7 = True
isDigitKey GLFW.Key'8 = True
isDigitKey GLFW.Key'9 = True
isDigitKey GLFW.Key'0 = True
isDigitKey _ = False

toDigitKey GLFW.Key'1 = Just 1
toDigitKey GLFW.Key'2 = Just 2
toDigitKey GLFW.Key'3 = Just 3
toDigitKey GLFW.Key'4 = Just 4
toDigitKey GLFW.Key'5 = Just 5
toDigitKey GLFW.Key'6 = Just 6
toDigitKey GLFW.Key'7 = Just 7
toDigitKey GLFW.Key'8 = Just 8
toDigitKey GLFW.Key'9 = Just 9
toDigitKey GLFW.Key'0 = Just 10
toDigitKey _ = Nothing

pressedDigits :: UIApp (Set.Set Int)
pressedDigits = fmap (Set.fromList  . mapMaybe toDigitKey . Set.toList) UI.pressedKeys

arrowKey2nav k =
  case k of
    GLFW.Key'Up -> Just DParent
    GLFW.Key'Down -> Just DChild
    GLFW.Key'Right -> Just DNext
    GLFW.Key'Left -> Just DPrev



-- TODO :

-- preview of pressed axes
-- faces of the holes
-- taileEditMode
-- preview of IExpr

-- showing subfaces of codim > 0 while filling is turned off
-- "animated" filling

-- show context
-- add curent goal to context
-- type ITail
-- for ITail defined
-- "zoom" into cell

-- TO DECIDE :
-- how axes preview should work




allKeys :: [GLFW.Key]
allKeys = [minBound .. maxBound]
