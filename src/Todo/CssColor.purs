-- | CssColor and animations thereof
module Todo.CssColor
where

import Prelude

import Data.Array (range)
import Data.Int (hexadecimal, toStringAs)
import Data.String (length)


-- | Contains the RGB values as Ints - should be between 0 and 0xFF
newtype CssColor =
  CssColor { red:: Int, green:: Int, blue:: Int }

instance showCssColor :: Show CssColor where
  show rgb =
    "#" <> colorStr clamped.red <> colorStr clamped.green <> colorStr clamped.blue
    where
      CssColor clamped = clampColor rgb
      colorStr color =
        let s = toStringAs hexadecimal color
        in  if length s < 2
              then "0" <> s
              else s

clampColor :: CssColor -> CssColor
clampColor (CssColor color) =
  CssColor
    { red: clampPart color.red
    , green: clampPart color.green
    , blue: clampPart color.blue }
  where
    clampPart i | i < 0 = 0
    clampPart i | i > 255 = 255
    clampPart i = i

gradient :: Int -> CssColor -> CssColor -> Array CssColor
gradient num (CssColor start) (CssColor end) =
  let
    rd = end.red - start.red
    gd = end.green - start.green
    bd = end.blue - start.blue
    color i =
      CssColor
        { red:   start.red + i*rd/num
        , green: start.green + i*gd/num
        , blue:  start.blue + i*bd/num}
  in
    map color (range 0 (num - 1))
