module Main where

import Lib
import Graphics.Gloss
import Numeric.LinearAlgebra.HMatrix

main :: IO ()
main
 = display
        (InWindow
               "Hello World"     -- window title
                (400, 150)       -- window size
                (10, 10))        -- window position
        white                    -- background color
        picture                  -- picture to display

txt :: String
txt = show (euclidianDistance (makePoint 1 2 3 4) (makePoint 5 6 7 8))



picture :: Picture
picture
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text txt     -- text to display
