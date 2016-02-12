{- | This is the Defensive strategy module.
-}
module ApocStrategyDefensive where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
defense    :: Chooser
defense b Normal        c = return (Just [(0,0),(2,1)])
defense b PawnPlacement c = return (Just [(2,2)])

