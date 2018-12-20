module Main where

import           Happlets.Lib.Gtk
import           Happlets.Games.Toddler.AsciiArt

----------------------------------------------------------------------------------------------------

main :: IO ()
main = happlet gtkHapplet $ do
  registeredAppName   .= "Toddler Games"
  windowTitleBar      .= "Toddler Games"
  recommendWindowSize .= (1024, 768)
  quitOnWindowClose   .= True

  mainWindow <- newWindow
  asciiArt   <- liftIO newAsciiArtGame
  attachWindow True mainWindow asciiArt startAsciiArtGame
