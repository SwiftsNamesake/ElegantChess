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
--        - Terminology (moves, pieces, source square and target square, capturing or attacking, 'ranks' for rows etc.)
--        - Castling, en passant

-- SPEC | - http://en.wikipedia.org/wiki/Rules_of_chess
--        -



module Chess.Core where



----------------------------------------------------------------------------------------------------
-- We'll need these
----------------------------------------------------------------------------------------------------
--https://hackage.haskell.org/package/grid-2.1.1/docs/Math-Geometry-Grid.html
--import Data.Vector ((//), (!), fromList, Vector)
import Data.Function (on)
import Data.Maybe (isJust, catMaybes)

import Control.Monad (liftM, liftM2, when)

import Text.Printf

import qualified IOUtil

--import Control.Monad (when)
--import Text.Printf



----------------------------------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------------------------------
-- TODO: Consider names for types (Square = Maybe Piece would make more sense)
data Piece    = Pawn | Rook | Bishop | Knight | Queen | King deriving (Eq, Show, Enum)
data Colour   = Black | White deriving (Eq, Show, Enum) -- TODO: Rename (?)
data Unit     = Unit Piece Colour deriving (Eq, Show) -- TODO: Rename (eg. chessman) (?)

type Square   = Maybe Unit -- TODO: Rename (?)
type Board    = [[Square]] --Vector (Vector Square)

newtype Row = Row Int deriving (Eq, Show) -- deriving (Num)
newtype Col = Col Int deriving (Eq, Show)
type Point = (Row, Col)

-- TODO: Move type with additional information (successful, capture, moved piece, captured piece)



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
-- | Replaces the element at the specified index with a new value
replace :: (Int, a) -> [a] -> [a]
replace (i, x) xs = let (before, after) = splitAt i xs in before ++ x : tail after


-- |
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
-- TODO: Return result instead of a new Board (eg. 'dry' update, success or failure, etc.)
-- |
move :: Board -> Point -> Point -> Board
move board from to = [[Nothing]]
	where source = at board from
	      target = at board to


-- |
at :: Board -> Point -> Square
at board (Row row, Col col) = board !! row !! col


-- Predicates for Squares, Pieces and Boards -------------------------------------------------------

-- TODO: Consistent function signatures (when feasible)
-- (eg. curried or uncurried coordinates, taking a Maybe Square or a column and row)

-- | Row and column is within range
inside :: Point -> Bool
inside (Row row, Col col) = (row >= 0) && (row < 8) && (col >= 0) && (col < 8)


-- | Inverse of inside (cf. above)
notInside :: Point -> Bool
notInside point = not $ inside point


-- | Determines if a square is occuped (ie. not empty)
occupied :: Square -> Bool
occupied = isJust


-- | hasEnemy
-- Compares the colours of the pieces on two (possibly empty) Squares (True when unequal).
-- TODO: Flesh out comment, more descriptive argument names
hasEnemy :: Square -> Square -> Bool
hasEnemy a b = maybe False id $ liftM2 ((/=) `on` colour) a b


-- | Inverse of hasEnemy (cf. above)
notEnemy :: Square -> Square -> Bool
notEnemy a b = not $ hasEnemy a b


-- | hasAlly
-- Compares the colours of the pieces on two (possibly empty) Squares (True when equal).
hasAlly :: Square -> Square -> Bool
hasAlly a b = maybe False id $ liftM2 ((==) `on` colour) a b


-- | Inverse of hasAlly (cf. above)
notAlly :: Square -> Square -> Bool
notAlly a b = not $ hasAlly a b


-- | Validates a move from one Square to a specific row and column.
-- 
-- Accepts a Board, Square and Point. A move is valid iff the Square at the given Point on the Board is either
-- empty or occupied by an enemy piece.
--
-- It is invalid if the Point is outside the Board.
--
-- TODO: Accept Points or Squares (?)
-- Won't work because of the bounds checking.
validMove :: Board -> Square -> Point -> Bool
validMove board from to = inside to && notAlly from (at board to)


-- | Validates a single step in a path of multiple steps.
--
-- This function takes a Board, a coordinate (row, col) indicating the next position, and the direction we're stepping in (delta col, delta row)
-- A step from one square to another (as indicated by the position and direction) is valid if the latter is not occupied by an allied piece,
-- and if the square we're about to leave wasn't previously occupied by an enemy piece (since one cannot continue moving after a capture).
-- Furthermore, one clearly cannot move outside the board, so the function also performs bounds checking.
--
-- TODO: Slight refactoring (clarify, remove parentheses)
validStep :: Board -> Point -> Point -> Bool
validStep board (Row dRow, Col dCol) (Row row, Col col) = validMove board from (Row row, Col col) && (notInside (Row $ row-dRow, Col $ col-dCol) || notEnemy from to)
	where from = at board (Row $ row-dRow, Col $ col-dCol)
	      to   = at board (Row row, Col col) -- Should not be evaluated if bounds checking fails


-- | Consumes a path (list of absolute row and column pairs) until the first invalid move
-- TODO: Flesh out comment
takeValid :: Board -> Point -> [Point] -> [Point]
takeValid board delta steps = takeWhile (validStep board delta) $ steps


-- | Creates a path (list of relative steps) from the given range, origin and direction
-- TODO: Accept an int (length) instead of a list (range)
path :: [Int] -> Point -> Point -> [Point]
path range (Row dRow, Col dCol) (Row row, Col col) = map step range -- TODO: Extract path logic
	where step n = (Row $ dRow*n+row, Col $ dCol*n+col)             -- Position at nth step (TODO: Rename)

-- | 
-- TODO: Include argument (?)
-- TODO: Verify
-- TODO: Add comment
filterInvalid :: Board -> Square -> [Point] -> [Point]
filterInvalid board square moves = filter (validMove board square) moves


-- Steps -------------------------------------------------------------------------------------------
-- These function (values?) yield lists of relative steps for each type of piece
--rook = 
--bishop =
--knight =
--queen =
--king =
--pawn =


-- | Initial row of a pawn
-- TODO: Generalize to startrow and direction
startrow :: Unit -> Row
startrow pawn = case colour pawn of
	Black -> Row 1
	White -> Row 6


-- TODO: transforming functions taking Square to function taking col row and board

-- | Creates paths based on a list of (row, column) pairs and an initial position
-- These pairs indicate the direction of each path
-- Each path is subsequently truncated at the first invalid step
-- The paths are then flattened into a single list of absolute (row, col) coordinates
-- TODO: Verify comment, flesh out if necessary
createPaths :: Board -> Point -> [Point] -> [Point]
createPaths board origin deltas = concat $ map createPath deltas
	where createPath delta = takeValid board delta $ path [1..7] delta origin


-- |
-- TODO: Add comments
-- TODO: Sort out naming conventions (will be easier once Square has become Maybe Unit or Maybe Piece)
-- TODO: Simplify
-- TODO: Verify
pieceMoves :: Board -> Point -> Unit -> [Point]
pieceMoves board from@(Row row, Col col) unit = case unit of
	Unit Rook _   -> fromDeltas [(-1, 0), (1, 0), (0, -1), (0, 1)] -- TODO: Extract path logic
	Unit Bishop _ -> fromDeltas [(-1, -1), (1, 1), (1, -1), (-1, 1)]
	Unit Knight _ -> fromAbsolute [(row+dy, col+dx) | dx <- [-1, 1, -2, 2], dy <- [-1, 1, -2, 2], dx /= dy ]
	Unit Queen _  -> fromDeltas . concat $ [[(-1, 0), (1, 0), (0, -1), (0, 1)], [(-1, -1), (1, 1), (1, -1), (-1, 1)]] -- TODO: Merge Rook and Knight
	Unit King _   -> fromAbsolute [(row+dy, col+dx) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0] -- TODO: Add two steps logic
	Unit Pawn _   -> fromAbsolute $ [(row+pawnDy, col+dx) | dx <- [-1, 0, 1], (dx == 0) || hasEnemy square (at' (Row $ row+pawnDy, Col $ col+dx))] ++ twosteps
	where at'        = at board                              -- TODO: Rename
	      fromDeltas = createPaths board origin . makePoints -- Creates a list of validated steps from the given directions (deltas)
	      fromAbsolute = filterInvalid board square . makePoints
	      square  = at' $ makePoint row col     -- Maybe Square, technically (cf. related TODOs)
	      origin  = (Row row, Col col)          --
	      makePoint rw cl = (Row rw, Col cl)    --
	      makePoints = map $ uncurry makePoint  --
	      twosteps = if Row row == pawnRow then [(row+2*pawnDy, col)] else []
	      pawnRow = startrow unit -- 
	      pawnDy | colour unit == Black =  1
	             | otherwise            = -1


-- |
-- TODO: Extract auxiliary definitions
-- TODO: Use Complex Int Int (?)
-- TODO: Rename, verb (?)
-- TODO: Change signature (?)
moves :: Board -> Int -> Int -> [Point]
-- TODO: Replace these definitions with curried versions of the global helper functions
moves board row col = let square = at board (Row row, Col col)
                      in maybe [] (pieceMoves board (Row row, Col col)) $ square


-- Lenses -----------------------------------------------------------------------------------------
-- |
-- TODO: Spelling (?)
colour :: Unit -> Colour
colour (Unit _ col) = col


-- |
piece :: Unit -> Piece
piece (Unit piece _) = piece


-- Transformations ----------------------------------------------------------------------------------
-- |
-- TODO: Simplify
-- TODO: Rename piece argument (clashes with function) (?)
showUnit :: Unit -> String
showUnit (Unit unit colour) = case unit of
	Pawn   -> ["♟♙" !! index]
	Rook   -> ["♜♖" !! index]
	Bishop -> ["♝♗" !! index]
	Knight -> ["♞♘" !! index]
	Queen  -> ["♛♕" !! index]
	King   -> ["♚♔" !! index]
	where indexOf Black = 0
	      indexOf White = 1
	      index = indexOf colour


-- showSquare


-- |
-- TODO: Simplify
-- TODO: Maybe Square (?)
-- TODO: Char or string
-- TODO: Only allow spaces for empty squares (?)
-- TODO: Define with guards for the constructor arguments and monadic chaining (>>=)
readSquare :: Char -> Square
readSquare c = liftM2 (,) piece col >>= (return . uncurry Unit)
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


-- |
-- TODO: Bounds checking (?)
readBoard :: String -> Board
readBoard str = map (map readSquare) . lines $ str



----------------------------------------------------------------------------------------------------
-- 
----------------------------------------------------------------------------------------------------

-- |
-- TODO: Create 2D list or vector instead (?)
grid :: Int -> Int -> (Int -> Int -> a) -> [a]
grid rw cl f = [f r c | r <- [0..rw-1], c <- [0..cl-1]]


-- | Logic and Graphics tests
mainDebug :: IO ()
mainDebug = do
	return ()



----------------------------------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------------------------------
-- |
runLogicTests :: IO ()
runLogicTests = do
	print "Everything seems to be in order."
	mapM_ (uncurry showMovesFor) [(4,4), (0,1), (6,6), (1, 2), (6, 3)]
	putStrLn "===== These tests should all evaluate to True ====================================="
	printf "Atleast one white: %s\n" . show . verifyCount $ White
	printf "Atkeast one black: %s\n" . show . verifyCount $ Black
	printf "Sixteen white pieces: %s\n" . show . verifySixteen $ White
	printf "Sixteen black pieces: %s\n" . show . verifySixteen $ Black 
	putStrLn "===== END OF TESTS ================================================================\n"
	print $ moves board 6 3
	IOUtil.putStr . take 8 $ initial
	IOUtil.putStr "λ κ γ π"
	where literal = map (\(r, c) -> (Row r, Col c)) [(0, 1), (0, 2)]
	      verifyCount col = any ((==col) . colour) . catMaybes $ concat board
	      verifySixteen col = (==16) . length . filter ((==col) . colour) . catMaybes $ concat board
	      printCols = putStrLn $ concatMap show [1..8]
	      board = readBoard initial
	      showMoves row col = putStrLn $ unlines [[marker row col rw cl | cl <- [0..7]] | rw <- [0..7] ]
	      showHeader square = printf "Moves for %s %s.\n" (show $ colour square) (show $ piece square)
	      pass = return () -- Alias for doing nothing
	      showMovesFor row col = maybe pass (\ sqr -> showHeader sqr >> printCols >> showMoves row col) $ at board (Row row, Col col) --(Square p c) <- at board row col
	      marker orow ocol row col | (Row row, Col col) `elem` moves board orow ocol = '█' -- Possible move
	                               | orow == row && ocol == col                      = 'O' -- Starting position
	                               | otherwise                                       = '.' -- Empty square
	                                                                       


----------------------------------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
	runLogicTests