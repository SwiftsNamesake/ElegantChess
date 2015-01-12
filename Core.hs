--
-- Core.hs
-- It's chess. Need I say more?
--
-- Jonatan H Sundqvist
-- January 10 2015
--

-- TODO | - Lenses
--        - Safe versions of each unsafe function (Maybe)

-- SPEC | -
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
data Piece  = Pawn | Rook | Bishop | Knight | Queen | King deriving (Eq, Show, Enum)
data Colour = Black | White deriving (Eq, Show, Enum) -- TODO: Rename (?)
data Square = Square Piece Colour deriving (Eq, Show)
type Board  = [[Maybe Square]] --Vector (Vector Square)

-- TODO: Consider names for types (Square = Maybe Piece would make more sense)



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
--
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


-- Row and column is within range
inside :: Int -> Int -> Bool
inside col row = (row >= 0) && (row < 8) && (col >= 0) && (col < 8)

notInside :: Int -> Int -> Bool
notInside col row = not $ inside col row

-- Determines if a square is occuped (ie. not empty)
occupied :: Maybe Square -> Bool
occupied = isJust

-- hasEnemy
-- Compares the colour of two pieces
hasEnemy :: Maybe Square -> Maybe Square -> Bool
hasEnemy a b = maybe False id $ liftM2 ((/=) `on` colour) a b

--
notEnemy :: Maybe Square -> Maybe Square -> Bool
notEnemy a b = not $ hasEnemy a b

-- hasAlly
hasAlly :: Maybe Square -> Maybe Square -> Bool
hasAlly a b = maybe False id $ liftM2 ((==) `on` colour) a b

--
notAlly :: Maybe Square -> Maybe Square -> Bool
notAlly a b = not $ hasAlly a b

--
validMove :: Board -> Maybe Square -> Int -> Int -> Bool
validMove board from x' y' = inside x' y' && notAlly from (at board x' y')


-- Steps -------------------------------------------------------------------------------------------
-- These function (values?) yield lists of relative steps for each type of piece
--rook = 
--bishop =
--knight =
--queen =
--king =
--pawn =

-- Initial row of a pawn
startrow :: Square -> Int
startrow pawn = case colour pawn of
	White -> 1
	Black -> 6


-- TODO: transforming functions taking Square to function taking col row and board

-- Creates paths based on a list of (dx, dy) pairs
-- These pairs indicate the direction of each path
-- Each path is subsequently truncated at the first invalid step
-- The paths are then flattened into a single list of absolute (col, row) coordinates
createPaths :: Board -> [(Int, Int)] -> [(Int, Int)]
createPaths deltas = concat $ map (\ delta -> takeValid delta $ path [1..7] delta) deltas

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
-- Entry point
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "█"