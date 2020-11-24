module Test where


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

import InteractiveParse

import Abstract

import Data.Map as Map

import Syntax

fn1 = "data/input-to-viz/penta-lhs"
fn1' = "data/input-to-viz/penta-rhs"
fn2 = "data/input-to-viz/debug-penta-lhs"
fn3 = "data/input-to-viz/debug2-penta-lhs"


loadFile :: String -> IO (Either String SessionState) 
loadFile fName =   
   liftIO $ do          
       handle <- (openFile fName ReadMode)
       contents <- hGetContents handle
       return $ parseInteractive contents 


sfCohTest :: String -> IO ()
sfCohTest fn =
  do res <- loadFile fn
     case res of
       Left err -> putStrLn (show err)
       Right res -> do
          putStrLn (show res)
          let (ee , expr0) = (ssEnvExpr res)
          let sf10 = substProj (snd ee) (Map.singleton 0 False) expr0
          putStrLn (toString ee sf10)
          return ()
-- ∧


main :: String -> IO ()
main fn =
  do res <- loadFile (fn)
     case res of
       Left err -> putStrLn (show err)
       Right res -> do
          -- putStrLn (show res)
          let (ee , expr0) = (ssEnvExpr res)
          let clc = toClCub ee expr0
          putStrLn (show clc)
          let expr1 = fromClCub ee clc

          putStrLn (toString ee expr0)
          putStrLn ("----")
          putStrLn (toString ee expr1)
          putStrLn ("----")
          putStrLn ("\nis same after conv?" ++
           (show (expr1 == expr0)) ++ "\n")
          return ()
-- ∧



  
