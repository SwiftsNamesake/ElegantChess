--
-- GTKTest.hs
-- Sandbox for GTK+ 3 experiments
--
-- Jonatan H Sundqvist
-- January 13 2015
--

-- TODO | - Carmichael numbers
--        -

-- SPEC | -
--        -

-- ghc --make GTKTest.hs -odir bin/ -o bin/GTKTest



import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad.Trans (liftIO)



hello :: (ButtonClass o) => o -> IO ()
hello b = do
	set b [buttonLabel := "Hello World: " ++ (show 5)]
	putStrLn "Changing button text"



oneButton :: IO ()
oneButton = do
	initGUI
	window <- windowNew
	button <- buttonNew
	scale  <- adjustmentNew 20 10 360 0.2 100 150
	set window [windowDefaultWidth := 60,
	            windowDefaultHeight := 30,
                containerChild := button,
                containerBorderWidth := 10]

	set button [buttonLabel := "Hola Mundo"]
	on button buttonActivated (hello button)
	on window objectDestroy mainQuit
	window `on` configureEvent $ do
		(width, height) <- eventSize
		liftIO $ do
			putStrLn (show width ++ " x " ++ show height)
			set button [buttonLabel := unwords ["Width :", show width, "| Height:", show height]]
		return False
	--onDestroy window mainQuit
	widgetShowAll window
	mainGUI



emptyWindow :: IO ()
emptyWindow = do
	initGUI
	window <- windowNew
	window `on` deleteEvent $ liftIO mainQuit >> return False
	-- i.e., on window deleteEvent (liftIO mainQuit >> return False)
	widgetShowAll window
	mainGUI


 
example :: IO ()
example = do
  initGUI
  window <- windowNew
  label <- labelNew $ Just "Hello, world from gtk2hs!"
  set window [ containerBorderWidth := 20,
               containerChild := label ]
  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI


withGlade :: IO ()
withGlade = do
  initGUI
  gui <- builderNew
  builderAddFromFile gui "C:/Users/Jonatan/Desktop/sampleOne.glade"
  window <- builderGetObject gui castToWindow "toplevel"
  window `on` deleteEvent $ liftIO mainQuit >> return False
  --void $ on window deleteEvent $ liftIO (mainQuit >> return False)
  widgetShowAll window
  mainGUI



main :: IO ()
main = withGlade