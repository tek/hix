module Hix.Ui.Data.Attr where

import Brick (attrMap)
import Brick.AttrMap (AttrMap, AttrName, attrName)
import Brick.Widgets.Border (borderAttr)
import Graphics.Vty (
  Attr,
  blue,
  bold,
  cyan,
  defAttr,
  green,
  linearColor,
  magenta,
  red,
  withBackColor,
  withForeColor,
  withStyle,
  yellow,
  )

import Hix.Ui.Data.Theme (SolarizedDark (..), solarizedDark)

newtype BrickAttr =
  BrickAttr AttrName
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

brickAttr :: BrickAttr -> AttrName
brickAttr = coerce

brickAttrs :: [(BrickAttr, a)] -> [(AttrName, a)]
brickAttrs = coerce

instance IsString BrickAttr where
  fromString = BrickAttr . attrName

-- | Attribute for help corner key descriptions (subdued).
helpDescAttr :: AttrName
helpDescAttr = brickAttr "help-desc"

-- | Attribute for help corner key bindings (slightly prominent).
helpKeyAttr :: AttrName
helpKeyAttr = brickAttr "help-key"

-- | Default attribute map for UI applications using Solarized Dark.
defaultAttrMap :: AttrMap
defaultAttrMap =
  attrMap defaultAttr (brickAttrs attrs <> rawAttrs)
  where
    sol = solarizedDark

    defaultAttr =
      defAttr `withForeColor` sol.base0 `withBackColor` sol.base03

    rawAttrs =
      [
        (borderAttr, withForeColor defAttr sol.base01)
      ]

    attrs =
      [
        ("focused", withStyle defAttr bold),
        ("focused" <> "tile", withBackColor defAttr (linearColor @Int 55 60 150)),
        ("bullet" <> "selected", withForeColor defAttr sol.green),
        ("bullet" <> "enabled", withForeColor defAttr sol.red),
        ("bullet" <> "disabled", withForeColor defAttr sol.base01),
        ("row" <> "disabled", withForeColor defAttr sol.base01),
        ("package-name", withForeColor defAttr sol.blue),
        ("version-digit", withForeColor defAttr sol.yellow),
        ("instruction", withForeColor defAttr sol.base1),
        -- Subdued step indicator
        ("instruction" <> "step", withForeColor defAttr sol.base01),
        -- Haskell purple for instruction indicator (adjusted for Solarized bg)
        ("instruction" <> "indicator", withForeColor defAttr (linearColor @Int 55 60 150)),
        ("chevrons", bolded (withForeColor defAttr sol.magenta)),
        ("help-desc", withForeColor defAttr sol.base01),
        ("help-key", withForeColor defAttr sol.yellow),
        ("muted", withForeColor defAttr sol.base01)
      ]

    bolded = flip withStyle bold

-- | Standard color attributes for non-solarized themes
standardAttrs :: [(BrickAttr, Attr)]
standardAttrs =
  [
    ("focused", bolded defAttr),
    ("focused" <> "tile", withForeColor defAttr blue),
    ("bullet" <> "selected", withForeColor defAttr green),
    ("bullet" <> "enabled", withForeColor defAttr red),
    ("chevrons", bolded (withForeColor defAttr magenta))
  ]
  where
    bolded = flip withStyle bold

-- | Test attributes with high contrast for debugging
testAttrs :: [(BrickAttr, Attr)]
testAttrs =
  [
    ("focused", withStyle (withBackColor defAttr yellow) bold),
    ("focused" <> "tile", withBackColor defAttr magenta),
    ("disabled", withBackColor defAttr cyan)
  ]
