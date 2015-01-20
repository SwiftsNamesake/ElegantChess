--
-- Chess.hs
-- It's chess. Need I say more?
--
-- Jonatan H Sundqvist
-- January 5 2015
--

-- TODO | - Lenses (cf. world argument to playIO)
--        - Assets, settings profiles (JSON)
--        - Clock
--        - Organize in modules and sub-modules (eg. Chess.Graphics and Chess.Core) (?)
--        - Cabal and Haddock (?)
--        - UI (Label, Entry, Button, etc.)
--        - Configrable Chess sets (different models)
--        - Custom chess set (perhaps with a theme)
--        - Rename Main (eg. play, main) (?)

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
--https://hackage.haskell.org/package/grid-2.1.1/docs/Math-Geometry-Grid.html
--import Graphics.UI.GLUT hiding (Color)
--import Control.Monad (when)
--import Data.Vector ((//), (!), fromList, Vector)
import Data.Function (on)

import qualified Data.Set as Set

import Control.Monad (when)

import Graphics.Gloss (display, circleSolid, rectangleSolid) -- hiding (Vector)
import Graphics.Gloss.Data.Picture (line)
import Graphics.Gloss.Interface.IO.Game -- hiding (Vector, color, Color) -- TODO: Use pure (?)

import Text.Printf



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Logic
---------------------------------------------------------------------------------------------------
data Language = Java | C | Python | Haskell deriving (Eq, Ord, Show)
contrast :: Language -> Language -> IO ()
contrast a b = printf "%s is a piece of shit compared to %s.\n" (show $ min a b) (show $ max a b)



---------------------------------------------------------------------------------------------------
-- Interaction
---------------------------------------------------------------------------------------------------
-- GLUT -------------------------------------------------------------------------------------------
{-
mainGLUT :: IO ()
mainGLUT = do
	(name, args) <- getArgsAndInitialize
	createWindow "Chess"
	displayCallback $= flush
	mainLoop
-}


-- chequer
squareColor :: Int -> Int -> Color
squareColor x y
	| ((==) `on` even) x y = black
	| otherwise = white


--
-- TODO: Extract coordinate system logic
squareSolid :: Float -> Float -> Float -> Picture
squareSolid side x y = translate (x*side-wd/2+side/2) (y*side-ht/2+side/2) $ rectangleSolid side side
	where (wd, ht) = (80*8, 80*8)


--
-- TODO: Make polymorphic (?)
chequer :: Float -> (Int -> Int -> Color) -> Int -> Int -> Picture
chequer side paint rw cl = color (paint rw cl) $ squareSolid side x y
	where (x, y) = (fromIntegral rw, fromIntegral cl)


--
-- TODO: Create 2D list or vector instead (?)
grid :: Int -> Int -> (Int -> Int -> a) -> [a]
grid rw cl f = [f r c | r <- [0..rw-1], c <- [0..cl-1]]


-- Gloss ------------------------------------------------------------------------------------------
--
mainGloss :: IO ()
mainGloss = playIO
	window -- Window mode
	white 	-- Background colour
	60		-- FPS (simulation steps per second, technically)
	world 	-- Initial world
	render	-- Converts world to Picture
	respond -- User interaction
	advance -- Advances the world to the next simulation step
	where
		window			 = InWindow "Chess" (width, height) (posX, posY)
		world			 = (Set.fromList [] :: Set.Set Key, (0, 0)) -- (key presses, mouse)
		render (p, m)	 = do
			let (mx, my) = m
			let hovered = chequer size (\_ _ -> red) (floor (mx + wd/2) `div` sz) (floor (my + ht/2) `div` sz) -- TODO: Extract coordinate conversion and hover logic
			when (Set.member (Char 'w') p) $ contrast Java Haskell
			return . pictures $ grid 8 8 (chequer size squareColor) ++ [hovered, text "A♜"]
		respond e (p, m) = return $ case e of
			EventKey key Down _ _ -> (Set.insert key p, m)
			EventKey key Up   _ _ -> (Set.delete key p, m)
			EventMotion mouse     -> (p, mouse)
			_ 			 -> (p, m)
		advance _ w		= return w
		(width, height)	= (floor size*8, floor size*8)              -- Window dimensions (Integral)
		(wd, ht)		= (fromIntegral width, fromIntegral height)	-- Window dimensions (Floating)
		size 			= 80 		 -- Size of a single square
		sz              = floor size -- Integer size
		(posX,  posY) 	= (125, 125) -- Window position


-- Logic and Graphics tests
mainDebug :: IO ()
mainDebug = do
	image <- loadBMP "assets/pieces.bmp"
	display window white image
	where size = (2*6*72+2*10, 94+2*10) -- Each glyph is (currently) 72x94 pixels. There are 12 glyphs in total (two per colour). Includes padding.
	      pos  = (0, 0)                 -- Position of the window's top left corner in relation to the monitor
	      window = InWindow "Pieces" size pos --


---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
-- TODO: Decide on a framework for the frontend (GLUT, OpenGL, Cairo, SDL)
main :: IO ()
main = do
	mainDebug
	mainGloss
	--putStrLn "Hello World"
	--putStr . visualise $ Square Rook Black