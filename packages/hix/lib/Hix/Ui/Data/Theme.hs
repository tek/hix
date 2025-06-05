module Hix.Ui.Data.Theme where

import Graphics.Vty (Color)
import Graphics.Vty.Attributes.Color (linearColor)

data Theme =
  Default
  |
  Native
  deriving stock (Eq, Show)

-- | Solarized Dark color palette
data SolarizedDark = SolarizedDark {
  base03  :: Color,  -- #002b36 - background
  base02  :: Color,  -- #073642 - background highlights
  base01  :: Color,  -- #586e75 - comments / secondary content
  base00  :: Color,  -- #657b83 - body text / default code
  base0   :: Color,  -- #839496 - body text / default code
  base1   :: Color,  -- #93a1a1 - optional emphasized content
  base2   :: Color,  -- #eee8d5 - background highlights (light)
  base3   :: Color,  -- #fdf6e3 - background (light)
  yellow  :: Color,  -- #b58900
  orange  :: Color,  -- #cb4b16
  red     :: Color,  -- #dc322f
  magenta :: Color,  -- #d33682
  violet  :: Color,  -- #6c71c4
  blue    :: Color,  -- #268bd2
  cyan    :: Color,  -- #2aa198
  green   :: Color   -- #859900
}

solarizedDark :: SolarizedDark
solarizedDark = SolarizedDark {
  base03  = rgb 0 43 54,
  base02  = rgb 7 54 66,
  base01  = rgb 88 110 117,
  base00  = rgb 101 123 131,
  base0   = rgb 131 148 150,
  base1   = rgb 147 161 161,
  base2   = rgb 238 232 213,
  base3   = rgb 253 246 227,
  yellow  = rgb 181 137 0,
  orange  = rgb 203 75 22,
  red     = rgb 220 50 47,
  magenta = rgb 211 54 130,
  violet  = rgb 108 113 196,
  blue    = rgb 38 139 210,
  cyan    = rgb 42 161 152,
  green   = rgb 133 153 0
}
  where
    rgb :: Word8 -> Word8 -> Word8 -> Color
    rgb r g b = linearColor r g b
