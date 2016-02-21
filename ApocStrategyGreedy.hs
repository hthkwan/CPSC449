{- | This is the Greedy strategy module.
-}
module ApocStrategyGreedy where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
greedy :: Chooser
greedy b Normal        c = return (Just (greedyMove c (theBoard b)))
greedy b PawnPlacement c = return (Just (getEmptySquare (theBoard b)))

greedyMove :: Player -> [[Cell]] -> [(Int, Int)]
greedyMove player board = getTheRightInput(getGreedyMove board (getAllMoves (getAllPieces player board) player board))


getAllPieces 					:: Player -> [[Cell]] -> [(Int, Int)]
getAllPieces player board 
			| player == White 	= [(x,y) | x <- [0..4], y <- [0..4], (getFromBoard board (x,y) == WK || getFromBoard board (x,y) == WP)]
			| otherwise 		= [(x,y) | x <- [0..4], y <- [0..4], (getFromBoard board (x,y) == BK || getFromBoard board (x,y) == BP)]
		
getAllMoves 							:: [(Int, Int)] -> Player -> [[Cell]] -> [((Int, Int),(Int, Int))]
getAllMoves ((x,y):xs) player board 
			| getFromBoard board (x,y) == WK 	= [((x,y),(w,v)) | w <- [x-2..x+2], v <- [y-2..y+2]]
			| getFromBoard board (x,y) == WP 	= [((x,y),(w,v)) | w <- [x], v <- [y+1]]
			| getFromBoard board (x,y) == BK 	= [((x,y),(w,v)) | w <- [x-2..x+2], v <- [y-2..y+2]]
			| getFromBoard board (x,y) == BP 	= [((x,y),(w,v)) | w <- [x], v <- [y-1]]
			| otherwise 						= [((x,y),(w,v)) | w <- [x], v <- [y]]
	
getGreedyMove :: [[Cell]] -> [((Int,Int),(Int,Int))] -> ((Int, Int),(Int,Int))
getGreedyMove board (((x,y),(w,v)):xs)
		| getFromBoard board (x,y) == WK && getFromBoard board (w,v) == BP		= ((x,y),(w,v))
		| getFromBoard board (x,y) == BK && getFromBoard board (w,v) == WP 		= ((x,y),(w,v))
		| getFromBoard board (x,y) == WP && getFromBoard board (w,v) == BP		= ((x,y),(w,v))
		| getFromBoard board (x,y) == BP && getFromBoard board (w,v) == WP		= ((x,y),(w,v))
		| getFromBoard board (w,v) == E 										= ((x,y),(w,v))
		| otherwise																= getGreedyMove board xs

getTheRightInput :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
getTheRightInput ((x,y),(w,v)) = [(x,y),(w,v)]


getEmptySquare :: [[Cell]] -> [(Int, Int)]
getEmptySquare board = [(x,y) | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == E]
