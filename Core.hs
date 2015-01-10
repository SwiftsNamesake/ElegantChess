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

import qualified Data.Set as Set

--import Control.Monad (when)
--import Text.Printf



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
data Piece  = Pawn | Rook | Bishop | Knight | Queen | King deriving (Eq, Show, Enum)
data Color  = Black | White deriving (Eq, Show, Enum) -- TODO: Rename (?)
data Square = Square Piece Color deriving (Eq, Show)
type Board  = [[Maybe Square]] --Vector (Vector Square)



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
move board (fx, fy) (tx, ty) = let from = (board ! fy) // [(fx, at board tx ty)]
                                   to   = (board ! ty) // [(tx, at board fx fy)]
                               in board // [(ty, from), (fy, to)]

--
at :: Board -> Int -> Int -> Square
at board row col = board ! row ! col


-- Lenses -----------------------------------------------------------------------------------------
-- 
-- TODO: Spelling (?)
color :: Square -> Color
color (Square _ col) = col

-- 
piece :: Square -> Piece
piece (Square piece _) = piece


-- Transformations ----------------------------------------------------------------------------------
--
-- TODO: Simplify
showPiece :: Square -> String
showPiece (Square piece color) = case piece of
	Pawn   -> ["♟♙" !! index]
	Rook   -> ["♜♖" !! index]
	Bishop -> ["♝♗" !! index]
	Knight -> ["♞♘" !! index]
	Queen  -> ["♛♕" !! index]
	King   -> ["♚♔" !! index]
	where indexOf Black = 0
	      indexOf White = 1
	      index = indexOf color

--
-- TODO: Simplify
-- TODO: Maybe Square (?)
-- TODO: Char or string
readPiece :: Char -> Maybe Square
readPiece c
	| c `elem` "♟♙" = Just $ Square colour Pawn
	| c `elem` "♜♖" = Just $ Square colour Rook
	| c `elem` "♝♗" = Just $ Square colour Bishop
	| c `elem` "♞♘" = Just $ Square colour Knight
	| c `elem` "♛♕" = Just $ Square colour Queen
	| c `elem` "♚♔" = Just $ Square colour King
	| otherwise = Nothing
	where colour
		| c `elem` white = White
		| c `elem` black = Black
		| otherwise      = error "Invalid piece" -- This will never happen



readBoard :: String -> Board
readBoard str = map 


board :: Board
board = fromList $ map fromList pieces


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