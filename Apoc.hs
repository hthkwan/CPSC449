{- | This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module Main(main) where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.List
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import ApocStrategyGreedy
import ApocStrategyDefensive


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    -- This is where the arguments are parsed into a placeholder list.
    strat <- parseArgs args

    print initBoard

    -- This is where they are used, 
    move <- (strat !! 0) (initBoard) Normal Black
    move' <- (strat !! 1) (initBoard) Normal White

    putStrLn (show $ GameState (if move==Nothing
                                then Passed
                                else Played (head (fromJust move), head (tail (fromJust move))))
                               (blackPen initBoard)
                               (Passed)
                               (whitePen initBoard)
                               (replace2 (replace2 (theBoard initBoard)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E))


{- | Helper function for parseArgs to convert a specified string input into the
     correct Chooser type for the game.
-}
stringToChooser :: String -> Chooser
stringToChooser a = case a of
    "human" -> human
    "greedy" -> greedy
    "defense" -> defense

{- | Takes the command line arguments (if any are passed) and returns the
     appropriate list of strategies. At position 0 is the Black strategy, and
     position 1 is the White strategy.
-}
parseArgs :: [String] -> IO([GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])])
parseArgs s 
    | (length s) == 2   = do
        return [(stringToChooser (head s)),(stringToChooser (head(tail s)))]
    | otherwise         = do
        putStrLn "Possible Strategies:\n\thuman\n\tgreedy\n\tdefense"
        putStrLn "Enter the strategy for BLACK:"
        black <- getLine
        putStrLn black
        putStrLn "Enter the strategy for WHITE:"
        white <- getLine
        putStrLn white
        return [(stringToChooser black), (stringToChooser white)]

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

