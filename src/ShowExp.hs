{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ShowExp where
import Control.Monad.Reader      (ReaderT , ask , reader)
import Control.Monad.Writer      (WriterT , tell , mapWriterT , listen)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put , mapRWST)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Control.Applicative (liftA2)

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
   , asCub                :: ClCub ()
   , asDrawMode           :: DrawExprMode
   , asViewport           :: Viewport
   , asDragStartVP        :: Maybe Viewport
   , asUserMode           :: UserMode
   , asMarkedCAddresses   :: [ CAddress ]
   , asDragStartAddress   :: Maybe Address
   , asKeyPressed         :: Bool
   , asMessage            :: String
   , asDisplayPreferences :: DisplayPreferences
   , asTime               :: DiffTime
   , asLastMousePos      :: (Double , Double)
   }

asCursorAddress :: AppState -> Maybe Address
asCursorAddress as =
  let um = (asUserMode as) in
  case um of
    UMNavigation {} ->
      let ca = umCoursorAddress um in Just $ ca        
    UMSelectGrid {} -> Just $ umEditedCell (um)
    Idle -> Nothing
    UMHoleConflict {} -> Just $ umEditedCell (um)
    -- UMEditTail {} -> Just $ umEditedCell (um)
    -- UMAddSubFace {} -> Just $ fst $ umSubFaceToAdd (um)

asSecCursorAddress :: AppState -> Maybe Address
asSecCursorAddress as =
  let um = (asUserMode as) in
  case um of
    UMNavigation {} ->
      fmap mnAddr $ umModalNav um        
    UMSelectGrid {} -> umHighlightedCell um
    _ -> Nothing
    -- UMEditTail {} -> Nothing
    -- UMAddSubFace {} -> Nothing

asSubFaceToAdd :: AppState -> Maybe (Address , SubFace)
asSubFaceToAdd as =
    case asUserMode as of
      -- UMAddSubFace x -> Just x
      _ -> Nothing

asExpression = ssEnvExpr . asSession

-- asCub :: AppState -> (ClCub ())
-- asCub = toCub . asExpression

asViewportProc :: AppState -> Viewport
asViewportProc appS =
  case getDim $ asCub appS of
    2 -> Viewport 0 0 0
    _ -> asViewport appS


data ModalNav = MNSub Address | MNSup Address

mnAddr (MNSub a) = a 
mnAddr (MNSup a) = a 

data UserMode =
    Idle
  | UMNavigation { umCoursorAddress :: Address , umModalNav :: Maybe ModalNav }
  -- | UMAddSubFace { umSubFaceToAdd :: (Address , SubFace) }
  -- | UMEditCell { umEditedCell :: Address }
  -- | UMEditTail { umEditedCell :: Address , umTailPosition :: Int }
  | UMSelectGrid { umEditedCell :: Address , umHighlightedCell :: Maybe Address , umGSD :: GridSelectionDesc }
  | UMHoleConflict { umEditedCell :: Address , -- <-- not necesarly same as CAddress in umFillHoleConflict
                     umFillHoleConflict :: (SubstAtConflict () ()) , umMbResolution :: Maybe SubstAtConflictResolutionStrategy }
    
umNavigation :: Address -> UserMode
umNavigation addr = UMNavigation { umCoursorAddress = addr , umModalNav = Nothing }

umAllowSelect :: UserMode -> Bool
umAllowSelect UMNavigation {} = True
umAllowSelect Idle = True
umAllowSelect _ = False

umModalNavigation :: ClCub () -> ModalNav -> UserMode
umModalNavigation cub z =
   UMNavigation { umCoursorAddress = rotateFrom (mnAddr z) (mnOptions cub z)  , umModalNav = Just z } 

umEnsureModalNavigation :: ClCub () -> ModalNav -> UserMode -> UserMode
umEnsureModalNavigation cub z (um@UMNavigation {}) =
  case (z , umModalNav um) of
    ((MNSub _) , Just (MNSub _)) -> um
    ((MNSup _) , Just (MNSup _)) -> um
    _ -> umModalNavigation cub z


rotateModalNav :: ClCub () -> Bool -> UserMode -> UserMode
rotateModalNav cub b (um@UMNavigation {}) =
    let Just z = umModalNav um in
    um { umCoursorAddress = rotateFromDir b (umCoursorAddress um) (mnOptions cub z) }


-- TODO: refactor to operate on Set
mnOptions :: ClCub () -> ModalNav -> [Address]
mnOptions cub (MNSub addr) = 
  addr : [ addressSubFace addr (toSubFace fc) | fc <- genAllLI (addresedDim addr) ]
mnOptions cub (MNSup addr) = addr :
  (fmap (head . Set.toList . cAddress) $ Set.toList (addressClassSuperCellsClass cub (addressClass cub addr)))
  -- addressSuperCells cub addr

umModalNavigationOptions :: ClCub () -> UserMode -> [Address]
umModalNavigationOptions cub = fromMaybe [] . (fmap $ mnOptions cub) . umModalNav
      -- case umModalNav um of
      --   Nothing -> Just $ ca
      --   Just (MNSub i) ->
      --     let fcsAddrss = [ addressSubFace ca (toSubFace fc) | fc <- genAllLI (addresedDim ca) ]
      --     in Just $ cycle (ca : fcsAddrss) !! i
      --   Just (MNSup i) -> Just $ umCoursorAddress um


data DrawExprMode = Stripes | StripesNoFill | Scaffold | Scaffold2 | Constraints | DebugDEM
  deriving (Show , Eq)

drawExprModes = [
  -- Stripes ,
    Scaffold
  , Constraints
  , DebugDEM
  ]

drawExpr :: AppState -> DrawExprMode -> (Env , Context) -> ClCub ()
                  -> (Drawing (([String] , ExtrudeMode) , Color))
-- drawExpr _ Stripes = fmap pure . mkDrawExprFill DefaultPT
-- drawExpr _ StripesNoFill = fmap pure . mkDrawExpr DefaultPT 
-- drawExpr _ Scaffold = \e ->
--    sequence $  [
--                 mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True , sptCursorAddress = Nothing })
--               , mkDrawExprFill (ScaffoldPT { sptDrawFillSkelet = False  , sptCursorAddress = Nothing })              
--               ] <*> pure e



drawExpr as Scaffold ee e =

   let (sptCA , sptSCA , sptMSFC) =
          case (asSubFaceToAdd as) of
             Nothing -> ((fmap (\a -> (a , addressClass (asCub as) a)) $ asCursorAddress as  )
                          , asSecCursorAddress as  , Nothing)
             Just x -> (Nothing , Nothing , Just x)

   in concat (

              [                
                mkDrawCub (CursorPT { cptCursorAddress = sptCA
                                     , cptSecCursorAddress = sptSCA
                                        })

                ,
                mkDrawCub (DefaultPT { dptCursorAddress = fmap fst sptCA
                          , dptShowFill = dpShowFilling $ asDisplayPreferences as
                          , dptFillFactor = 1.0
                              -- 0.5 * (sin (realToFrac $ asTime as) + 1)

                          })
                
               , \ee e ->
                    fmap (second $ const (([] , Basic ) , nthColor 3))
                           (mkDrawCub FaceHandles ee e) 
                

              -- ,
              --   mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True
              --                              , sptCursorAddress = sptCA
              --                              , sptMissingSubFaceCursor = sptMSFC
              --                              , sptScaffDim = 1})
              

              ] <*> (pure ee) <*> (pure e)
               -- ++
               -- maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
              ) 

drawExpr as Constraints ee e =

   let (cub , sptCA , sptSCA ) =
          case (asCursorAddress as) of 
             -- Just a -> ((fmap fst $ traceConstraints (asCub as) (cAddrWithSubFaces (asCub as) (addressClass (asCub as) a)))
             --             , (Just ((a , addressClass (asCub as) a)) )
             --              , asSecCursorAddress as)

             -- Just a -> ((fmap fst $ traceConstraints (asCub as) (Set.singleton (addressClass (asCub as) a)))
             --             , (Just ((a , addressClass (asCub as) a)) )
             --              , asSecCursorAddress as)

             Just a -> ((fmap fst $ traceConstraintsSingle (asCub as) ((addressClass (asCub as) a)))
                         , (Just ((a , addressClass (asCub as) a)) )
                          , asSecCursorAddress as)


             Nothing -> (fmap (const Nothing) cub , Nothing , Nothing)

       

   in concat (

              [                
                mkDrawCub (ConstraintsViewPT { cvptDrawFillSkelet = True
                                           , cvptCursorAddress = fmap fst sptCA
                                           , cvptScaffDim = 1
                                           , cvptCub = cub })
              
              ] <*> (pure ee) <*> (pure e)
               -- ++
               -- maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
              ) 


drawExpr as DebugDEM ee e =
        concat (

              [                
                fmap (second $ const (([] , Basic ) , nthColor 3)) (mkDrawCub ClickPoints ee e)

              ] 
               -- ++
               -- maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
              ) 


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

render
  :: GLFW.Window
     -> Int
     -> Int
     -> [Descriptor]
     -> RWST UI.Env [uiMsg] (UI.State AppState glDesc uiMsg) IO ()
render win w h descriptors =
   do
      viewport <- UI.getsAppState asViewportProc
      liftIO $ onDisplayAll win w h viewport descriptors
      currTime <- fmap utctDayTime $ liftIO getCurrentTime
      UI.modifyAppState (\s -> s { asTime = currTime })
      -- UI.flushDisplay
      return ()

initialize :: WriterT [Msg] IO (Maybe AppState)
initialize =
  do args <- liftIO getArgs

     (mbFName , mbInitialSessionState) <- 
        case args of
          fName : _ -> do r <- liftIO (loadFile fName)
                          case r of
                             Left e -> return (Nothing , Left e)
                             Right ss -> return (Just fName , Right ss)
                          
          [] -> return (Nothing , Right $ freshSessionState 3)
         
     currTime <- fmap utctDayTime $ liftIO getCurrentTime

     case mbInitialSessionState of
       Right initialSessionState -> do
         let (ee , expr) = ssEnvExpr initialSessionState
         return $ Just $
                   AppState
                    { fileName          = mbFName
                    , asViewport        = Viewport { vpAlpha = 1/5 , vpBeta = 0 , vpGamma = -0.1 }
                    , asDrawMode        = Scaffold -- Stripes --   --head drawExprModes
                    , asDragStartVP     = Nothing
                    , asSession         = initialSessionState
                    , asCub             = toClCub ee expr
                    , asUserMode        = Idle --umNavigation (rootAddress (getDim ee))
                    , asKeyPressed      = False
                    , asMessage         = ""
                    , asDisplayPreferences = defaultDisplayPreferences
                    , asTime            = currTime
                    , asMarkedCAddresses = []
                    , asDragStartAddress = Nothing
                    , asLastMousePos = (0,0)
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

     exprR <- (parseExpr ctx) <$> liftIO (getLine)
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

setGridHighlightAddr :: Address -> UIApp ()
setGridHighlightAddr addr = undefined


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
     return (globalInfo  ++ "\n" ++ modalInfo)

  where


    keHelp :: (UI.Event -> UIAppDesc ()) -> (GLFW.Key -> UIAppDesc ())
    keHelp f k = do
        win <- reader UI.envWindow
        f (UI.EventKey win k 0 GLFW.KeyState'Pressed
                          (GLFW.ModifierKeys False False False False False False) )

    appDescHelp x =
        fmap ((intercalate "\n") . (fmap (\(k , inf) -> "[" ++ k ++ "]: " ++ inf))) (fmap (fmap (first (drop 4 . show)) . (filter (not . null . snd)))
      $ UI.collect allKeys (extractInfo . (keHelp $ x) ))

    infoStr :: (GLFW.Key , String) -> String
    infoStr = show

consolePrint :: String -> UIApp ()
consolePrint str = do
  UI.modifyAppState (\s ->
    s { asMessage = str })
  UI.flushDisplay


asTailAddress :: AppState -> Maybe Int
asTailAddress as =
  case asUserMode as of
    -- UMEditTail {} -> Just $ umTailPosition (asUserMode as)
    _ -> Nothing

printExpr :: UIApp ()
printExpr =
  do appS <- UI.getAppState
     cub <- UI.getsAppState asCub
     (ee , expr) <- UI.getsAppState asExpression
     addrs <- UI.getsAppState asCursorAddress

     liftIO
          $ (putStrLn $
       (printCub ee (CubPrintData {cpdCursorAddress = addrs , cpdTailAddress = asTailAddress appS}) cub))




modifyDrawingPreferences :: (DisplayPreferences -> DisplayPreferences) -> UIApp ()
modifyDrawingPreferences f = do
    UI.modifyAppState (\s ->
               let ss = asDisplayPreferences s
               in s {
                       asDisplayPreferences =
                       f ss
                                })
    UI.flushDisplay



setFromCub :: ClCub () -> UIApp ()
setFromCub newCub = do
    setFromCubNoFlush newCub
    UI.flushDisplay

setFromCubNoFlush :: ClCub () -> UIApp ()
setFromCubNoFlush newCub = do
    UI.modifyAppState (\s ->
               let ss = asSession s
                                      
                   newS = ssSetExpression ss (fromClCub (fst (ssEnvExpr ss) ) newCub)
                   --um = maybe um ( \addr -> if isValidAddressFor newCub addr then um else Idle) $ asCursorAddress s
                     
                     
               in s { --asUserMode = um ,
                      asSession = newS
                     , asCub = newCub -- toCub $ ssEnvExpr newS
                                })



transformExpr :: CubTransformation -> UIApp ()
transformExpr trns =
   do
      appS <- UI.getAppState
      case applyTransform trns (asCub appS) of
        (Left (CubTransformationConflict fhc)) ->
             initHoleConflict (toAddress $ ct2CAddress trns) fhc
          
        (Left err) ->
            consolePrint $ "transform error: " ++ show err
        (Right newCub) -> do setFromCubNoFlush $ void newCub
                             setUserMode (Idle)    
                             
printExprCode :: UIApp ()
printExprCode =
  do (ee , expr) <- UI.getsAppState asExpression
     liftIO $ do SCA.clearScreen
                 putStrLn
                    $ fromRight "error while printing expr" (toCode ee expr)

-- setAddSubFaceMode :: Address -> UIApp ()
-- setAddSubFaceMode addr = do
--             appS <- UI.getAppState
--             let cub = asCub appS
--             let vsf = Set.toList $ vaccantSubFaces cub addr
--             case vsf of
--                  [] -> return ()
--                  (x : _) -> setUserMode (UMAddSubFace (addr , x))

--                -- Just _ -> do UI.modifyAppState $ \s -> s { asSubFaceToAdd = Nothing }
--                --              updateView

setNavMode :: Address -> UIApp ()
setNavMode addr = setUserMode (umNavigation addr)


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
          UI.modifyAppState (\s -> s { asLastMousePos = (x , y) })  
          mouseD <- gets UI.stateMouseDown
          x0 <- gets UI.stateDragStartX
          y0 <- gets UI.stateDragStartY

          when mouseD
            (do s <- UI.getsAppState id
                case asDragStartVP s of
                   Nothing -> return ()
                   Just vp0 -> do
                     let newV  = (asViewport s)
                             { vpAlpha = vpAlpha vp0 - (realToFrac (y - y0) / 2000.0)
                             , vpGamma = vpGamma vp0 + (realToFrac (x - x0) / 2000.0) }
                     -- liftIO $ putStrLn $ show newV 
                     UI.setAppState $ s { asViewport = newV }
            )
            
        (UI.EventMouseButton _ mb mbs mk) -> inf "" $
           when (mb == GLFW.MouseButton'1) $ do
            let pressed = mbs == GLFW.MouseButtonState'Pressed
                released = mbs == GLFW.MouseButtonState'Released
            dsVP <- UI.getsAppState asDragStartVP
            -- draggingView
            when (pressed) $ do
              if (GLFW.modifierKeysShift mk)
              then when (isNothing dsVP ) $ UI.modifyAppState $ \s -> s
                      { asDragStartVP = Just (asViewport s) }
              else do
                 mbAddr' <- getHoveredAddress
                 UI.modifyAppState (\s -> s { asDragStartAddress = mbAddr' })
                 
            when (released) $ do
              um <- UI.getsAppState asUserMode
              cub <- UI.getsAppState asCub
              mbAddr' <- getHoveredAddress
              mbSda <- UI.getsAppState asDragStartAddress
              when (umAllowSelect um && mbAddr' == mbSda && isJust mbSda) $ do                   
                   UI.modifyAppState (\s -> s { asUserMode = umNavigation $ fromJust mbSda })
                   UI.flushDisplay
              case (um , mbSda , mbAddr') of
                 (UMNavigation addrSel _ , Just addr0 , Just addr1 ) -> do
                     -- consolePrint (show (addr0 , addr1 ))
                     case (mbSelectFaces cub addrSel addr0 addr1) of
                        Just x -> do dragFaceOntoFace x
                                     UI.flushDisplay
                        Nothing -> return ()
                
                 _ -> return ()


            when ((not pressed) && isJust dsVP) $ UI.modifyAppState $ \s -> s
                   { asDragStartVP = Nothing }
                
        (UI.EventKey win k scancode ks mk) ->
              when (ks == GLFW.KeyState'Pressed) $ do
                          -- Q, Esc: exit
            when (k == GLFW.Key'Escape) $ inf "Quit" $
              liftIO $ GLFW.setWindowShouldClose win True
            when (k == GLFW.Key'P) $ inf "Print Expression" $
              printExprCode
            when (k == GLFW.Key'F) $ inf "Toggle Fillings" $
              modifyDrawingPreferences
                (\dp ->
                   dp { dpShowFilling = not $ dpShowFilling dp })
        _ -> return ()





modesEvents :: PEMode -> UserMode -> UI.Event -> UIAppDesc ()

modesEvents _ Idle ev = return ()

modesEvents pem um@UMNavigation { umCoursorAddress = addr } ev = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
         addrClass = addressClass cub addr
     handleCellManipulationEvents pem ev addr
     case ev of

        -- (UI.EventMouseButton _ mb mbs mk) -> inf "" $
           -- when (mb == GLFW.MouseButton'1) $ do
           --   let pressed = mbs == GLFW.MouseButtonState'Pressed
           --   when (pressed && (not $ GLFW.modifierKeysShift mk)) $ do

              
            -- dsVP <- UI.getsAppState asDragStartVP
            -- -- draggingView
            -- when (pressed) $ do
            --   if (GLFW.modifierKeysShift mk)
            --   then when (isNothing dsVP ) $ UI.modifyAppState $ \s -> s
            --           { asDragStartVP = Just (asViewport s) }
            --   else return ()

            -- when ((not pressed) && isJust dsVP) $ UI.modifyAppState $ \s -> s
            --        { asDragStartVP = Nothing }
       
        (UI.EventKey win k scancode ks mk) -> do
             when (ks == GLFW.KeyState'Pressed) $ do
                -- when (k == GLFW.Key'H) $ inf "EditHead" $ do
                --   initEditHead addr

                when (k == GLFW.Key'G) $ inf "GiveCell" $ do
                  initGiveCell addrClass

               
                when (k == GLFW.Key'M) $ do
                  if (GLFW.modifierKeysControl mk)
                  then inf "Un-mark cells" $ unMarkAll
                  else inf "Mark cell" $ markCell addrClass
                  


                -- when (k == GLFW.Key'T) $ do
                --   initEditTail pem addr

                when (k == GLFW.Key'V) $ inf "CycleDrawMode" $ do
                  UI.modifyAppState (\s -> s { asDrawMode = rotateFrom (asDrawMode s) drawExprModes } )

                when (k == GLFW.Key'A) $ do
                  case (isHcompSideAddrToAdd cub addr) of
                    Just (addr , sf) -> inf "Add side cell" $  transformExpr (AddSide (addressClass cub addr) sf )
                    Nothing -> return ()


                -- when (k == GLFW.Key'C) $ do
                --   if (GLFW.modifierKeysControl mk)
                --   then inf "Delete sub-face with subfaces" $ transformExpr (RemoveCell addr)
                --   else inf "Delete sub-face" $ transformExpr (RemoveCellLeaveFaces addr)
                --   inf "" $ setNavMode (fullSF (getDim (head addr)) : (tailAlways addr))

                -- when (k == GLFW.Key'S) $ inf "Split cell" $ do
                --   transformExpr (SplitCell addr)

                when (k == GLFW.Key'Backspace) $ do
                  if (GLFW.modifierKeysControl mk)
                  then inf "Insert Hole Interior" $ transformExpr (ClearCell addrClass WithFreeSubFaces )
                  else inf "Insert Hole WithFreeFaces" $ transformExpr (ClearCell addrClass OnlyInterior )



                when (k == GLFW.Key'Delete) $
                  case (isHcompSideAddr addr) of
                     Nothing -> return ()
                     Just (addrClass' , sf) -> inf "Delete Side in Hcomp" $ do
                                 transformExpr (RemoveSide (addressClass cub addrClass') (sf , RSRKeepLeaves))

                when (k == GLFW.Key'S) $ do
                  if (GLFW.modifierKeysControl mk)
                  then inf "todo" $ transformExpr (ClearCell addrClass WithFreeSubFaces )
                  else inf "Insert Hcomp" $ transformExpr (SplitCell addrClass)  


                when (isArrowKey k) $ do
                  if (GLFW.modifierKeysControl mk)

                  then inf "" $ do               
                       UI.modifyAppState (\s ->
                          let (axis , dir) = fromJust (arrowKey2BoolBool k)
                              mn = case axis of
                                       True -> MNSup addr
                                       False -> MNSub addr
                          in s { asUserMode = rotateModalNav cub dir (umEnsureModalNavigation cub mn (asUserMode s))}  
                          )
                       UI.flushDisplay

                  else inf "" $  do               
                       UI.modifyAppState (\s ->
                          let nav = fromJust (arrowKey2nav k)
                              jna = fromRight (addr) (cubNav (asCub appS) addr nav)

                          in s { asUserMode = umNavigation jna })
                       UI.flushDisplay


             when (ks == GLFW.KeyState'Released) $ do
                when (k == GLFW.Key'LeftControl && isJust ( umModalNav $ asUserMode appS ) ) $ inf "..." $ do
                   UI.modifyAppState (\s ->
                           s { asUserMode = umNavigation addr })
                   UI.flushDisplay 
        _ -> return ()


-- modesEvents pem um@(UMEditCell addr  ) ev = undefined

-- modesEvents pem um@(UMEditTail addr pos ) ev = do
--      appS <- UI.getAppState
--      let cub = asCub appS
--          inf = helperPEM pem

--          localCub = clCubPickO addr cub

--          tail =
--             case localCub of
--                   Right (Cub _ _ (Just (CellExpr vI tail'))) -> tail'

--                   _ -> error "??"

--      case ev of
--         (UI.EventKey win k scancode ks mk) ->
--             when (ks == GLFW.KeyState'Pressed) $ do
--               when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
--                  -- transformExpr (AddSubFace (addr , sf))
--                  setUserMode (umNavigation addr)

--               when (k == GLFW.Key'Q) $ inf "Abort" $ do
--                 setUserMode $ umNavigation addr

--               when (k == GLFW.Key'T) $ inf "Cycle Tail Positions" $ do
--                 setUserMode $ (UMEditTail addr (mod (pos + 1) (length tail) ) )

--                   -- when (isArrowKey k) $ inf "nav" $ do

--                   --    let nav = fromJust (arrowKey2nav k)
--                   --        vcnt = Set.toList $ vaccantSubFaces cub addr
--                   --        sfNext =
--                   --           case nav of
--                   --              DNext -> rotateFrom sf vcnt
--                   --              DPrev -> rotateFrom sf $ reverse vcnt
--                   --              _ -> sf

--                   --    setUserMode $ UMAddSubFace (addr , sfNext)


--         _ -> return ()


-- modesEvents pem um@(UMAddSubFace (addr , sf) ) ev = do
--      appS <- UI.getAppState
--      let cub = asCub appS
--          inf = helperPEM pem
--      case ev of
--         -- (UI.EventKey win k scancode ks mk) ->
--         --     when (ks == GLFW.KeyState'Pressed) $ do
--         --       when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
--         --          transformExpr (AddSubFace (addr , sf))
--         --          setUserMode (UMNavigation { umCoursorAddress = sf : addr })

--         --       when (k == GLFW.Key'Q) $ inf "Abort" $ do
--         --         setUserMode $ UMNavigation addr


--         --       when (isArrowKey k) $ inf "nav" $ do

--         --          let nav = fromJust (arrowKey2nav k)
--         --              vcnt = Set.toList $ vaccantSubFaces cub addr
--         --              sfNext =
--         --                 case nav of
--         --                    DNext -> rotateFrom sf vcnt
--         --                    DPrev -> rotateFrom sf $ reverse vcnt
--         --                    _ -> sf

--         --          setUserMode $ UMAddSubFace (addr , sfNext)


--         _ -> return ()

modesEvents pem um@(UMHoleConflict {} ) ev = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
     case ev of
        (UI.EventKey win k scancode ks mk) ->
            when (ks == GLFW.KeyState'Pressed) $ do
              when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
                case (umMbResolution um) of
                  Nothing -> setUserMode $ umNavigation (umEditedCell um)
                  Just res -> do setFromCubNoFlush $ void $ resolveConflict (umFillHoleConflict um) res 
                                 setUserMode $ umNavigation (umEditedCell um)

              when (k == GLFW.Key'Q) $ inf "Abort" $ do
                setUserMode $ umNavigation (umEditedCell um)

              when (isArrowKey k) $ inf "nav" $ do

                 let dir = arrowKey2Bool k

                 setUserMode $ um {
                   umMbResolution = rotateFromDir dir (umMbResolution um)
                   ((Nothing : (fmap Just [minBound ..])) :: [Maybe SubstAtConflictResolutionStrategy])
                   }

        _ -> return ()

modesEvents pem um@(UMSelectGrid addr addr2 gsd ) ev = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem
     case ev of
        (UI.EventKey win k scancode ks mk) ->
            when (ks == GLFW.KeyState'Pressed) $ do
              when (k == GLFW.Key'Enter) $ inf "Confirm" $ do
                setUserMode (umNavigation addr)
                gsdRunFinalAction gsd


              when (k == GLFW.Key'Q) $ inf "Abort" $ do
                gsdRunAbortAction gsd
                setUserMode $ umNavigation addr


              when (isArrowKey k) $ inf "nav" $ do

                 let nav = fromJust (arrowKey2nav k)

                 gsdRunDirection gsd nav


        _ -> return ()


rotationIndexes :: Address -> UIApp (Maybe (Int , Int))
rotationIndexes addr = do
  appS <- UI.getAppState
  pd <- pressedDims addr
  let n = getDim $ fromRight (error "wrong addrress") $ clCubPickO addr $ asCub appS
  return $ case n of
              2 -> Just (0 , 1)
              3 -> case Set.toList pd of
                        [ 0 ] -> Just (1,2)
                        [ 1 ] -> Just (0,2)
                        [ 2 ] -> Just (0,1)
                        _ -> Nothing
              _ -> Nothing

pressedDims :: Address ->  UIApp (Set.Set Int)
pressedDims addr = do
    appS <- UI.getAppState
    pd <- pressedDigits
    let n = getDim $ fromRight (error "wrong addrress") $ clCubPickO addr $ asCub appS
    let dimsL = (Set.map $ flip (-) 1) pd
    return $ Set.filter (n >) dimsL


handleCellManipulationEvents :: PEMode -> UI.Event -> Address -> UIAppDesc ()
handleCellManipulationEvents pem ev addr = do
     appS <- UI.getAppState
     let cub = asCub appS
         inf = helperPEM pem


     pd <- liftPEM $ pressedDims addr

     ri <- liftPEM $ rotationIndexes addr


     case ev of
        -- (UI.EventKey win k scancode ks mk) ->
        --      when (ks == GLFW.KeyState'Pressed) $ do
        --      when (k == GLFW.Key'I && length pd > 0) $ inf "negate dimensions" $ do
        --          transformExpr (MapAt addr (Right . negateDimIndexes pd))
        --      when (k == GLFW.Key'R && isJust ri) $ inf "rotate dimensions" $ do
        --          transformExpr (MapAt addr (Right . rotateDimIndexes (fromJust ri)))

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
modalConsoleView um@(UMSelectGrid addr addr2 gsd ) = do
  let cho = gridMapWithIndex (\ij -> if ij == gsdPosition gsd then SCP.bgColor SCP.Yellow else id ) (gsdChoices gsd)
      choStr = concatMap ((++) "\n" . concat . fmap ((++) " ") ) cho
  liftIO $ putStrLn choStr

modalConsoleView um@(UMHoleConflict {}) = do
  let cho = intercalate "," $ map (\x -> let t = maybe "abort" show x
                                         in if x == umMbResolution um then SCP.bgColor SCP.Yellow t else t
                                           )
                 $ ((Nothing : (fmap Just [minBound ..])) :: [Maybe SubstAtConflictResolutionStrategy])
            
  liftIO $ putStrLn "unify conflict resolution :"
  liftIO $ putStrLn cho
  
modalConsoleView _ = return ()

drawingInterpreter :: DrawingInterpreter ColorType
drawingInterpreter =
  DrawingInterpreter []
   (\case
      2 -> Just (toRenderablesIgnore . embed 2 (const 0))
      3 -> Just toRenderablesIgnore



      _ -> Nothing)

updateGL :: UIApp [Descriptor]
updateGL =
   do appState <- UI.getAppState
      let drawings = drawExpr appState (asDrawMode appState)
                      (fst (asExpression appState))
                      ((asCub appState))
      printConsoleView
      do let tryToRen = toRenderableDI drawingInterpreter drawings
         case tryToRen of
           Left msg -> error msg
           Right rens ->

             liftIO $ initResources $ snd rens

   where
     printConsoleView :: UIApp ()
     printConsoleView = do
        liftIO SCA.clearScreen
        printExpr
        ei <- eventsInfo
        liftIO $ putStrLn ei
        liftIO $ putStrLn ""

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
       handle <- openFile fName ReadMode
       contents <- hGetContents handle
       return $ parseInteractive contents


-- initEditTail :: PEMode -> Address -> UIAppDesc ()
-- initEditTail pem addr = do
--     appS <- UI.getAppState
--     let inf = helperPEM pem
--         cubO = fromRight (error "bad addr") $ clCubPickO addr $ asCub appS
--     case cubO of
--       (Cub _ _ (Just (CellExpr vI (_ : _)))) ->
--            inf "Edit Tail" $ setUserMode $ UMEditTail addr 0
--       _ -> return ()

-- initEditHead :: Address -> UIApp ()
-- initEditHead addr = do
--   appS <- UI.getAppState
--   let (ee0@(env , ctx0@(Context vars _)) , _) = asExpression appS
--       initialCub = fromMaybe (error "bad addr") $ clCubPick addr (asCub appS)
--       ctx = contextAt ctx0 addr (asCub appS)
--       varsInCtx = binsBy (getCTyDim env ctx . snd . fst) $ zip (reverse vars) (fmap VarIndex [0..])
--       varsInCtxGrid = fmap snd $ Map.toList varsInCtx

--       (initPos , initTail) =
--          case clInterior initialCub of
--             Cub _ _ (Just (CellExpr vI tail)) ->
--                (fromJust $ gridIndexOf vI $ fmap (fmap snd) varsInCtxGrid ,
--                   tail)
--             _ -> ((0,0) , [])

--       gsd = GridSelectionDesc
--        { gsdChoices = fmap (fmap (fst . fst)) varsInCtxGrid
--        , gsdPosition = initPos
--        , gsdAction =
--            \_ nPos -> do
--              let newVi = snd $ gridLookup nPos varsInCtxGrid
--              let newCell = mkCellExprForceDefTail (env , ctx) newVi initTail
--              undefined --transformExpr (ReplaceAt addr (Cub () _ (Right newCell)))
--        , gsdFinalAction = \_ _ -> return ()
--        , gsdAbortAction = transformExpr undefined --(ReplaceAt addr initialCub)
--        }
--   gsdAction gsd (gridLookup initPos (gsdChoices gsd)) initPos
--   setUserMode $ UMSelectGrid addr gsd


-- initEditTail :: PEMode -> Address -> UIAppDesc ()
-- initEditTail pem addr = do
--     appS <- UI.getAppState
--     let inf = helperPEM pem
--         cubO = fromRight (error "bad addr") $ clCubPickO addr $ asCub appS
--     case cubO of
--       (Cub _ _ (Just (CellExpr vI (_ : _)))) ->
--            inf "Edit Tail" $ setUserMode $ UMEditTail addr 0
--       _ -> return ()

unMarkAll :: UIApp ()
unMarkAll = UI.modifyAppState (\s -> s { asMarkedCAddresses = [] })

markCell :: CAddress -> UIApp ()
markCell x = UI.modifyAppState (\s -> s { asMarkedCAddresses = pushUniq x $ asMarkedCAddresses s })


dragFaceOntoFace :: (CAddress , (Face , Face)) -> UIApp () 
dragFaceOntoFace (caddrPar , ((fc1@(Face _ (k1 , b1)) , fc2@(Face _ (k2 , b2))))) = do
  cub <- UI.getsAppState asCub
  appS <- UI.getAppState
  
  let caddr0 = (cAddressSubFace cub caddrPar (toSubFace fc1))
      caddr1 = (cAddressSubFace cub caddrPar (toSubFace fc2))

      (ee0@(env , ctx0@(Context vars _)) , _) = asExpression appS

      cub0 = (fromJust $ clCubPick (toAddress caddr0) cub)
      cub1 = (fromJust $ clCubPick (toAddress caddr1) cub)
        
  when (isHoleClass cub caddrPar) $ do


       let n = cAddresedDim caddrPar
       if k1 == k2
       then let cubToDeg = cub0 --either (const cub0) id (unifyClCub cub0 cub1)
                cubD = clDegenerateTest k1 cubToDeg
            in transformExpr (SubstAt caddrPar cubD)
       else do let cub1' = if (b1 == b2)
                           then cub1
                           else negateDim (fromJust (punchOut k2 k1)) cub1  
                   cubToDeg = cub0 --either (const cub0) id (unifyClCub cub0 cub1')
                   cubD = substDimsClCub n (fmap Right (cornerTail fc1 fc2)) cubToDeg
               transformExpr (SubstAt caddrPar cubD)



initGiveCell :: CAddress -> UIApp ()
initGiveCell caddr = do
  appS <- UI.getAppState

  let addr = head $ Set.toList (cAddress caddr)
      holeDim = addresedDim addr
      (ee0@(env , ctx0@(Context vars _)) , _) = asExpression appS
      ctx = contextAt ctx0 addr (asCub appS)
      varsInCtxWithMatchingDim = filter
             (\x -> holeDim == (getCTyDim env ctx $ snd $ fst x)) $ zip (reverse vars) (fmap VarIndex [0..])
             
  when (isHoleClass (asCub appS) caddr && (not $ null varsInCtxWithMatchingDim)) $ do

    let 

        
        finalGive x = transformExpr (SubstAt caddr (toClCubAt env ctx0 (asCub appS) caddr x ))


      
        initGiveOptions :: VarIndex  -> UIApp ()
        initGiveOptions newVi = do
            let opts = case holeDim of
                          0 -> error "should be handled wihout second grid screen"
                          1 -> [ ("i", [ dim 0 ] ) , ("~i" , [ neg (dim 0) ] ) ]
                          2 -> error "todo"
                gsd' opts = GridSelectionDesc
                   { gsdChoices = [fmap fst opts]
                        
                   , gsdPosition = (0,0)
                   , gsdAction =
                       \_ nPos -> do
                         -- let newVi = snd $ gridLookup nPos varsInCtxGrid
                         -- let newCell = mkCellExprForceDefTail (env , ctx) newVi initTail
                         -- undefined --transformExpr (ReplaceAt addr (Cub () _ (Right newCell)))
                         return () 
                   , gsdFinalAction =
                       \_ nPos -> do
                         let newTail = snd $ gridLookup nPos [opts]
                         let newCell = mkCellExprForceDefTail (env , ctx) newVi newTail
                         finalGive newCell
                         
                   , gsdAbortAction = return ()
                     -- transformExpr undefined --(ReplaceAt addr initialCub)
                   }

            setUserMode $ UMSelectGrid addr Nothing  (gsd' opts)

      
        varsInCtxGrid = [ varsInCtxWithMatchingDim ]

        (initPos , initTail) = ((0,0) , [])
    
        gsd = GridSelectionDesc
         { gsdChoices = fmap (fmap (fst . fst)) varsInCtxGrid
         , gsdPosition = initPos
         , gsdAction =
             \_ nPos -> do
               -- let newVi = snd $ gridLookup nPos varsInCtxGrid
               -- let newCell = mkCellExprForceDefTail (env , ctx) newVi initTail
               -- undefined --transformExpr (ReplaceAt addr (Cub () _ (Right newCell)))
               return ()
         , gsdFinalAction = \gDStr nPos ->             
             let newVi = snd $ gridLookup nPos varsInCtxGrid
             in case holeDim of
                   0 -> finalGive (CellExpr newVi [])
                   1 -> initGiveOptions newVi             
                   _ -> error $ "not implemented dim" ++ (show holeDim);   --else initGiveOptions
         , gsdAbortAction = return ()
           -- transformExpr undefined --(ReplaceAt addr initialCub)
         }
    gsdAction gsd (gridLookup initPos (gsdChoices gsd)) initPos
    setUserMode $ UMSelectGrid addr Nothing gsd
    

getHoveredAddress :: UIApp (Maybe Address)
getHoveredAddress = do
  appS <- UI.getAppState 
  let (x , y) = asLastMousePos appS
      ((ee , ctx ) , _) = asExpression appS
      cub = asCub appS
  w <- gets UI.stateWindowWidth
  h <- gets UI.stateWindowHeight
  case getDim $ asCub appS of
    2 -> do let (mX , mY) = mouseToModel2d w h (x , y)
                cutoffDist = 100.0 / fromIntegral h
                clickPoints =   mkDrawCub ClickPoints (ee , ctx) cub                 
                              & fmap (first head)
                              & filter (\([x' , y'] , _ ) -> (abs (x' - mX) < cutoffDist) && (abs (y' - mY) < cutoffDist) )
                mDist ([x' , y'] , _ ) = (x' - mX) ^ 2 + (y' - mY) ^ 2
            -- consolePrint $ show (length clickPoints)
            return $
              case clickPoints of
                  [] -> Nothing
                  _ -> Just $ fst $ fst $ snd $ minimumBy (compare `on` mDist)
                               clickPoints
            
    _ -> return Nothing

-- (Nothing : (fmap Just [minBound ..]))

initHoleConflict :: Address -> (SubstAtConflict () ()) -> UIApp ()
initHoleConflict addr fhc = do
  setUserMode $ UMHoleConflict
     { umEditedCell = addr
     , umFillHoleConflict = fhc
     , umMbResolution = Nothing }
  UI.flushDisplay

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
gridIndexOf a ([] : xs) = (first ((+) 1)) <$> gridIndexOf a xs
gridIndexOf a (x : xs) =
  case elemIndex a x of
    Nothing -> gridIndexOf a ([] : xs)
    Just j -> Just (0 , j)


gridMapWithIndex :: ((Int , Int) -> a -> a) -> [[a]] -> [[a]]
gridMapWithIndex f = zipWith (\i -> zipWith (\j -> f (i , j) ) [0..]) [0..]

gsdRunFinalAction :: GridSelectionDesc -> UIApp ()
gsdRunFinalAction gsd =
  gsdFinalAction gsd (gridLookup (gsdPosition gsd) (gsdChoices gsd)) (gsdPosition gsd)

gsdRunAbortAction :: GridSelectionDesc -> UIApp ()
gsdRunAbortAction = gsdAbortAction

-- gsdRunAction :: GridSelectionDesc -> AppState
-- gsdRunAction gsd =
--   gsdAction gsd (gridLookup (gsdPosition gsd) (gsdChoices gsd)) (gsdPosition gsd)

gsdRunDirection :: GridSelectionDesc -> Direction -> UIApp ()
gsdRunDirection gsd d =
  let newPos = mv d


  in do
        gsdAction gsd (gridLookup newPos (gsdChoices gsd)) newPos
        UI.modifyAppState (\appS -> appS { asUserMode =
                                (\(UMSelectGrid addr _ gsd) ->
                                     UMSelectGrid addr Nothing
                                      (gsd { gsdPosition = newPos } ) )
                                 (asUserMode appS) } )
        UI.flushDisplay

  where

    (i , j) = gsdPosition gsd

    cho = gsdChoices gsd

    mv DNext = (i , mod (j + 1) (length (cho !! i)))
    mv DPrev = (i , mod (j - 1) (length (cho !! i)))
    mv DParent = (mod (i - 1) (length cho) , mod j (length (cho !! (i - 1))))
    mv DChild = (mod (i + 1) (length cho) , mod j (length (cho !! (i + 1))))

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

arrowKey2BoolBool k =
  case k of
    GLFW.Key'Up -> Just (True , True)
    GLFW.Key'Down -> Just (True , False)
    GLFW.Key'Right -> Just (False , True)
    GLFW.Key'Left -> Just (False , False)

arrowKey2Bool k =
  case k of
    GLFW.Key'Up -> False
    GLFW.Key'Down -> True
    GLFW.Key'Right -> True
    GLFW.Key'Left -> False



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
