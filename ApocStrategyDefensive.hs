{- | This is the Defensive strategy module.
-}
module ApocStrategyDefensive where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import ApocTools


defensive :: Chooser
defensive b Normal        c = return (Just (defensiveMove c (theBoard b)))
defensive b PawnPlacement c = return (Just (getEmptySquare (theBoard b)))

defensiveMove :: Player -> [[Cell]] -> [(Int, Int)]
defensiveMove player board = getTheRightInput(getDefensiveMove board (getAllMoves (getAllPieces player board) player board))


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
	
getDefensiveMove :: [[Cell]] -> [((Int,Int),(Int,Int))] -> ((Int, Int),(Int,Int))
getDefensiveMove board (((x,y),(w,v)):xs)
		| getFromBoard board (x,y) == WK && getFromBoard board (x, y+3) /= E	= getDefensiveMove board xs
		| getFromBoard board (x,y) == WK && getFromBoard board (x, y-3) /= E 	= getDefensiveMove board xs
		| getFromBoard board (x,y) == WP && getFromBoard board (x, y+2) /= E	= getDefensiveMove board xs
		| getFromBoard board (x,y) == BK && getFromBoard board (x, y+3) /= E	= getDefensiveMove board xs
		| getFromBoard board (x,y) == BK && getFromBoard board (x, y-3) /= E 	= getDefensiveMove board xs
		| getFromBoard board (x,y) == BP && getFromBoard board (x, y-2) /= E	= getDefensiveMove board xs
		| otherwise																= ((x,y),(w,v))

getTheRightInput :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
getTheRightInput ((x,y),(w,v)) = [(x,y),(w,v)]


getEmptySquare :: [[Cell]] -> [(Int, Int)]
getEmptySquare board = [(x,y) | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == E]