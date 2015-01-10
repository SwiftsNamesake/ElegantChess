--
-- Core.hs
-- It's chess. Need I say more?
--
-- Jonatan H Sundqvist
-- January 10 2015
--

-- TODO | - Lenses
--        - 

-- SPEC | -
--        -



module Core where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
--https://hackage.haskell.org/package/grid-2.1.1/docs/Math-Geometry-Grid.html
--import Data.Vector ((//), (!), fromList, Vector)
import Data.Function (on)
import Control.Monad (liftM)

import qualified Data.Set as Set

--import Control.Monad (when)
--import Text.Printf



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
data Piece  = Pawn | Rook | Bishop | Knight | Queen | King deriving (Eq, Show, Enum)
data Colour = Black | White deriving (Eq, Show, Enum) -- TODO: Rename (?)
data Square = Square Piece Colour deriving (Eq, Show)
type Board  = [[Maybe Square]] --Vector (Vector Square)

-- TODO: Consider names for types (Square = Maybe Piece would make more sense)



---------------------------------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------------------------------
initial :: String
initial = unlines [ "♖♘♗♕♔♗♘♖",
					"♙♙♙♙♙♙♙♙",
					"        ",
					"        ",
					"        ",
					"        ",
					"♟♟♟♟♟♟♟♟",
					"♜♞♝♛♚♝♞♜"]


white = "♜♞♝♛♚♟" -- All white pieces
black = "♖♘♗♕♔♙" -- All black pieces



---------------------------------------------------------------------------------------------------
-- Logic
---------------------------------------------------------------------------------------------------
-- Queries and updates ----------------------------------------------------------------------------
--
-- TODO: Simplify (perhaps with lenses)
-- TODO: More powerful query and update functions
move :: Board -> (Int, Int) -> (Int, Int) -> Board
move board (fx, fy) (tx, ty) = [[Nothing]]

--
at :: Board -> Int -> Int -> Maybe Square
at board row col = board !! row !! col


-- Lenses -----------------------------------------------------------------------------------------
-- 
-- TODO: Spelling (?)
colour :: Square -> Colour
colour (Square _ col) = col

-- 
piece :: Square -> Piece
piece (Square piece _) = piece


-- Transformations ----------------------------------------------------------------------------------
--
-- TODO: Simplify
showPiece :: Square -> String
showPiece (Square piece colour) = case piece of
	Pawn   -> ["♟♙" !! index]
	Rook   -> ["♜♖" !! index]
	Bishop -> ["♝♗" !! index]
	Knight -> ["♞♘" !! index]
	Queen  -> ["♛♕" !! index]
	King   -> ["♚♔" !! index]
	where indexOf Black = 0
	      indexOf White = 1
	      index = indexOf colour

--
-- TODO: Simplify
-- TODO: Maybe Square (?)
-- TODO: Char or string
-- TODO: Only allow spaces for empty squares (?)
-- TODO: Define with guards for the constructor arguments and monadic chaining (>>=)
readPiece :: Char -> Maybe Square
readPiece c = ((liftM (,)) col) >>= (liftM ($ piece)) >>= (return . uncurry Square)
  where piece
          | c `elem` "♟♙" = Just Pawn
          | c `elem` "♜♖" = Just Rook
          | c `elem` "♝♗" = Just Bishop
          | c `elem` "♞♘" = Just Knight
          | c `elem` "♛♕" = Just Queen
          | c `elem` "♚♔" = Just King
          | otherwise = Nothing
        col
          | c `elem` white = Just White
          | c `elem` black = Just Black
          | otherwise      = Nothing



readBoard :: String -> Board
readBoard str = map (map readPiece) str


---------------------------------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------------------------------

--
-- TODO: Create 2D list or vector instead (?)
grid :: Int -> Int -> (Int -> Int -> a) -> [a]
grid rw cl f = [f r c | r <- [0..rw-1], c <- [0..cl-1]]


-- Logic and Graphics tests
mainDebug :: IO ()
mainDebug = do
	return ()



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	return ()