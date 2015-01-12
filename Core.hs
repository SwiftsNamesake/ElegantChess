--
-- Core.hs
-- It's chess. Need I say more?
--
-- Jonatan H Sundqvist
-- January 10 2015
--

-- TODO | - Lenses
--        - Safe versions of each unsafe function (Maybe)
--        - Use types to catch col/row errors (cf. newtype) (?)
--        - Terminology (moves, pieces, source square and target square, capturing or attacking, etc.)

-- SPEC | - http://en.wikipedia.org/wiki/Rules_of_chess
--        -



module Core where



----------------------------------------------------------------------------------------------------
-- We'll need these
----------------------------------------------------------------------------------------------------
--https://hackage.haskell.org/package/grid-2.1.1/docs/Math-Geometry-Grid.html
--import Data.Vector ((//), (!), fromList, Vector)
import Data.Function (on)
import Control.Monad (liftM, liftM2)

import Data.Maybe (isJust)

--import Control.Monad (when)
--import Text.Printf



----------------------------------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------------------------------
-- TODO: Consider names for types (Square = Maybe Piece would make more sense)
data Piece  = Pawn | Rook | Bishop | Knight | Queen | King deriving (Eq, Show, Enum)
data Colour = Black | White deriving (Eq, Show, Enum) -- TODO: Rename (?)
data Square = Square Piece Colour deriving (Eq, Show) -- TODO: Rename (?)
type Board  = [[Maybe Square]] --Vector (Vector Square)

newtype Row = Row Int
newtype Col = Col Int


----------------------------------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------------------------------
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



----------------------------------------------------------------------------------------------------
-- Auxiliary
----------------------------------------------------------------------------------------------------
-- Replaces the element at the specified index with a new value
replace :: (Int, a) -> [a] -> [a]
replace (i, x) xs = let (before, after) = splitAt i xs in before ++ x : tail after

--
-- TODO: Uncurrying multiple-argument functions
update :: [(Int, a)] -> [a] -> [a]
update pairs xs = foldr replace xs pairs



----------------------------------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------------------------------
-- Queries and updates ----------------------------------------------------------------------------
--
-- TODO: Simplify (perhaps with lenses)
-- TODO: More powerful query and update functions
move :: Board -> (Int, Int) -> (Int, Int) -> Board
move board (row, col) (row', col') = [[Nothing]]
	where source = at board row col
	      target = at board row' col'

--
at :: Board -> Int -> Int -> Maybe Square
at board row col = board !! row !! col


-- Predicates for Squares, Pieces and Boards -------------------------------------------------------

-- TODO: Consistent function signatures (when feasible)
-- (eg. curried or uncurried coordinates, taking a Maybe Square or a column and row)

-- Row and column is within range
inside :: Int -> Int -> Bool
inside col row = (row >= 0) && (row < 8) && (col >= 0) && (col < 8)

notInside :: Int -> Int -> Bool
notInside col row = not $ inside col row

-- Determines if a square is occuped (ie. not empty)
occupied :: Maybe Square -> Bool
occupied = isJust

-- hasEnemy
-- Compares the colours of the pieces on two (possibly empty) Squares (True when unequal).
hasEnemy :: Maybe Square -> Maybe Square -> Bool
hasEnemy a b = maybe False id $ liftM2 ((/=) `on` colour) a b


-- Inverse of hasEnemy (cf. above)
notEnemy :: Maybe Square -> Maybe Square -> Bool
notEnemy a b = not $ hasEnemy a b


-- hasAlly
-- Compares the colours of the pieces on two (possibly empty) Squares (True when equal).
hasAlly :: Maybe Square -> Maybe Square -> Bool
hasAlly a b = maybe False id $ liftM2 ((==) `on` colour) a b


-- Inverse of hasAlly (cf. above)
notAlly :: Maybe Square -> Maybe Square -> Bool
notAlly a b = not $ hasAlly a b


--
validMove :: Board -> Maybe Square -> Int -> Int -> Bool
validMove board from x' y' = inside x' y' && notAlly from (at board x' y')


-- Validates a single step in a path of multiple steps.
--
-- This function takes a Board, a coordinate (row, col) indicating the current position, and a direction to step in (delta row, delta row)
-- A step from one square to another (as indicates by the position and direction) is valid if the latter is not occupied by an allied piece,
-- and if the square we're about to leave wasn't previously occupied by an enemy piece (since one cannot continue moving after a capture).
-- Furthermore, one clearly cannot move outside the board, so the function also performs bounds checking.
validStep :: Board -> (Int, Int) -> (Int, Int) -> Bool
validStep board (x, y) (dx, dy) = validMove board square x y && (notInside (x-dx) (y-dy) || notEnemy square (at board (x-dx) (y-dy)))


takeValid delta steps = takeWhile (validStep delta) $ steps
path range (dx, dy) = map (\ a -> (dx*a, dy*a)) range -- TODO: Extract path logic
filterInvalid = filter (uncurry $ validMove board square)

-- Steps -------------------------------------------------------------------------------------------
-- These function (values?) yield lists of relative steps for each type of piece
--rook = 
--bishop =
--knight =
--queen =
--king =
--pawn =


-- Initial row of a pawn
-- TODO: Generalize to startrow and direction
startrow :: Square -> Int
startrow pawn = case colour pawn of
	White -> 1
	Black -> 6


-- TODO: transforming functions taking Square to function taking col row and board

-- Creates paths based on a list of (dx, dy) pairs
-- These pairs indicate the direction of each path,
-- Each path is subsequently truncated at the first invalid step
-- The paths are then flattened into a single list of absolute (row, col) coordinates
createPaths :: Board -> [(Int, Int)] -> [(Int, Int)]
createPaths board deltas = concat $ map (\ delta -> takeValid delta $ path [1..7] delta) deltas


--
pieceMoves :: Square -> [(Int, Int)]
pieceMoves piece = case piece of
	Square Rook _   -> createPaths [(-1, 0), (1, 0), (0, -1), (0, 1)] -- TODO: Extract path logic
	Square Bishop _ -> createPaths [(-1, -1), (1, 1), (1, -1), (-1, 1)]
	Square Knight _ -> filterInvalid [(row+dx, col+dy) | dx <- [-1, 1, -2, 2], dy <- [-1, 1, -2, 2], dx /= dy ]
	Square Queen _  -> concat . map createPaths $ [[(-1, 0), (1, 0), (0, -1), (0, 1)], [(-1, -1), (1, 1), (1, -1), (-1, 1)]]
	Square King _   -> filterInvalid [(row+dx, col+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0] -- TODO: Add two steps logic
	Square Pawn _   -> filterInvalid [(row+dx, col+1) | dx <- [-1, 0, 1], dx == 0 || hasEnemy square (at board (row+dx) (col+1))]


--
-- TODO: Extract auxiliary definitions
-- TODO: Use Complex Int Int (?)
moves :: Board -> Int -> Int -> [(Int, Int)]
-- TODO: Replace these definitions with curried versions of the global helper functions
moves board row col = let square = at board row col
                          validStep (dx, dy) (x, y) = validMove board square x y && (notInside (x-dx) (y-dy) || notEnemy square (at board (x-dx) (y-dy)))
                          takeValid delta steps = takeWhile (validStep delta) $ steps
                          path range (dx, dy) = map (\ a -> (dx*a, dy*a)) range -- TODO: Extract path logic
                          filterInvalid = filter (uncurry $ validMove board square)
                      in maybe [] pieceMoves $ square


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
readPiece c = liftM2 (,) piece col >>= (return . uncurry Square)
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
readBoard str = map (map readPiece) . lines $ str



----------------------------------------------------------------------------------------------------
-- 
----------------------------------------------------------------------------------------------------

--
-- TODO: Create 2D list or vector instead (?)
grid :: Int -> Int -> (Int -> Int -> a) -> [a]
grid rw cl f = [f r c | r <- [0..rw-1], c <- [0..cl-1]]


-- Logic and Graphics tests
mainDebug :: IO ()
mainDebug = do
	return ()



----------------------------------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------------------------------
runLogicTests :: IO ()
runLogicTests = do
	print "Everything seems to be in order."


----------------------------------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "█"