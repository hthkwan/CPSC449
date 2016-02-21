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

    -- This is where isValid was tested
    --let test = isValid (initBoard) Black 0 4 1 3
    --print test

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

---Our Functions--------------------------------------------------------------
{- |
-}
isValid                 ::GameState -> Player -> Int -> Int -> Int -> Int -> Bool
isValid a ply x y x' y'
    |((x' < 0 || y' < 0))||(x' > 4 || y' > 4) = False --destination is out of range
    |((x < 0 || y < 0)||(x > 4 || y > 4)) = False --origin is out of range
    |((getFromBoard (theBoard a) (x,y)) == E) = False --no piece at origin to move
    |((getFromBoard (theBoard a) (x',y')) == WP) || ((getFromBoard (theBoard a) (x',y')) == WK) = if ply == Black then True else False --destination occupied by your own piece
    |((getFromBoard (theBoard a) (x',y')) == BP) || ((getFromBoard (theBoard a) (x',y')) == BK) = if ply == White then True else False --destination occupied by your own piece
    |((getFromBoard (theBoard a) (x,y)) == BP || (getFromBoard (theBoard a) (x,y)) == BK) && (ply == White) = False -- moving a piece that's not yours
    |((getFromBoard (theBoard a) (x,y)) == WP || (getFromBoard (theBoard a) (x,y)) == WK) && (ply == Black) = False -- moving a piece that's not yours
    |((getFromBoard (theBoard a) (x,y)) == BP || (getFromBoard (theBoard a) (x,y)) == BK) && (ply == Black) = --destination is not reachable by the piece found in origin cell - black player
        if (((getFromBoard (theBoard a) (x,y)) == BP) && ((y' /= (y-1)) || (x /= x')))
        then False else if((getFromBoard (theBoard a) (x,y)) == BK) && (((x' == (x-2) || x' == (x+2)) && (y' == (y+1) || y' == (y-1))) || ((x' == (x-1) || x' == (x+1)) && (y' == (y+2)||y' == (y-2))))
                        then True else False
    |((getFromBoard (theBoard a) (x,y)) == WP || (getFromBoard (theBoard a) (x,y)) == WK) && (ply == White) = --destination is not reachable by the piece found in origin cell - black player
        if (((getFromBoard (theBoard a) (x,y)) == WP) && ((y' /= (y-1)) || (x /= x')))
        then False else if((getFromBoard (theBoard a) (x,y)) == WK) && (((x' == (x-2) || x' == (x+2)) && (y' == (y+1) || y' == (y-1))) || ((x' == (x-1) || x' == (x+1)) && (y' == (y+2)||y' == (y-2))))
                        then True else False
    |otherwise = True

{- |
-}
pieceNum            ::[[Cell]] -> Cell -> Int
pieceNum [] _       = 0
pieceNum (x:xs) BK  = (pieceNum' x BK) + pieceNum xs BK
pieceNum (x:xs) BP  = (pieceNum' x BP) + pieceNum xs BP
pieceNum (x:xs) WK  = (pieceNum' x WK) + pieceNum xs WK
pieceNum (x:xs) WP  = (pieceNum' x WP) + pieceNum xs WP

{- |
-}
pieceNum'           ::[Cell] -> Cell -> Int
pieceNum' [] _      = 0
pieceNum' (x:xs) BK = if (x == BK) then (1 + pieceNum' xs BK) else pieceNum' xs BK
pieceNum' (x:xs) BP = if (x == BP) then (1 + pieceNum' xs BP) else pieceNum' xs BP
pieceNum' (x:xs) WK = if (x == WK) then (1 + pieceNum' xs WK) else pieceNum' xs WK
pieceNum' (x:xs) WP = if (x == WP) then (1 + pieceNum' xs WP) else pieceNum' xs WP

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

