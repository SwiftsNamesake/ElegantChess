--
-- GTKTest.hs
-- Sandbox for GTK+ 3 experiments
--
-- Jonatan H Sundqvist
-- January 13 2015
--

-- TODO | -
--        -

-- SPEC | -
--        -



import Graphics.UI.Gtk



main :: IO ()
main = do
  initGUI
  window <- windowNew
  widgetShowAll window
  mainGUI