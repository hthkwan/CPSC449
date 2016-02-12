{- | This module is used for CPSC 449 for the Apocalypse assignment.

This is merely a skeleton to get you started.  It has VERY little functionality.

Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module ApocStrategyHuman where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human b Normal        c = return (Just [(0,0),(2,1)])
    --line <- getLine
    --let vals = convertToIntList line
    --return (Just [((vals !! 0),(vals !! 1)),((vals !! 2),(vals !! 3))])
human b PawnPlacement c = return (Just [(2,2)])

{- | Takes user input and converts it into the form
     [(int,int),(int,int)] for a Normal move.
-}
getHumanNormalMove :: IO ()
getHumanNormalMove = do
    putStrLn $ "Enter the move coordinates for player Black in the form \'srcX srcY destX destY\'"
        ++"\n(0 >= n >= 4, or just enter return for a \'pass\') B2:"
    line <- getLine
    let v = convertToIntList line

{- | Converts the input line to a list of Ints.
-}
convertToIntList :: String -> [Int]
convertToIntList = map read . words
