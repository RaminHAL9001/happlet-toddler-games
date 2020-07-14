module Main where

import           Happlets.Provider.Gtk2
import           Happlets.Games.Toddler.AsciiArt

----------------------------------------------------------------------------------------------------

main :: IO ()
main = happlet gtkHapplet $ do
  registeredAppName   .= "Toddler Games"
  initWindowTitleBar  .= "Toddler Games"
  recommendWindowSize .= (1024, 768)
  quitOnWindowClose   .= True

  game <- newHappletIO asciiArtGame
  attachWindow True game startAsciiArtGame
