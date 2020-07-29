module InteractiveParseTests where

import Syntax
import InteractiveParse

import System.IO  
import Control.Monad

import Data.List

import Data.Bifunctor

main = do  
        let list = []
        handle <- openFile "data/input-to-viz/penta-rhs" ReadMode
        contents <- hGetContents handle
        let parseResult = parseInteractive contents
        -- putStr $ either id (intercalate "\n" . map (uncurry (++) . second parr)) parseResult
        putStr $ either id (show) parseResult
        hClose handle   
