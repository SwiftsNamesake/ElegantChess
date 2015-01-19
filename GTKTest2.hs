module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Layout.Rpn
import Data.Monoid


main = do
     initGUI

     -- Create a new operator  that takes a label
     -- and a button  and pack then
     -- in a horizontal box .

     let line s = mconcat [
          rLABEL s,
          rRIGHT,
          rBUTTON s $ return (),
          rCENTER,
          rHBOX [PackGrow,PackGrow]
          ]

     -- Create a new operator  that packs many 'line'
     -- in a vertical box .

     let column = mconcat $ [
          line "One Button",
          line "Another Button",
          line "Yet Another Button",
          line "Last Button",
          rVBOX $ replicate 4 PackGrow
          ]

     -- Take two copies of 'column' and add  then
     -- to a notebook . Then create a stock button
     --  and attach a action to an event . Then
     -- join the notebook and that button in a vertical box.
     -- Insert everything in a main window , and
     -- finally make everything into widgets .

     [mainWindow] <- widgetsFromRpn [
          column,
          column,
          rNOTEBOOK ["First Page","Second Page"],
          rBUTTON stockQuit mainQuit,
          rVBOX [PackGrow,PackNatural],
          rMAIN "Layout.Rpn Example"
          ]

     widgetShowAll mainWindow
     mainGUI