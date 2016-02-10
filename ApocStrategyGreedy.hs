module ApocStrategyGreed where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
greed    :: Chooser
greed b Normal        c = do
    line <- getLine
    let vals = convertToIntList line
    return (Just [((vals !! 0),(vals !! 1)),((vals !! 2),(vals !! 3))])
greed b PawnPlacement c = return (Just [(2,2)])

{- | Converts the input line to a list of Ints.
-}
convertToIntList :: String -> [Int]
convertToIntList = map read . words