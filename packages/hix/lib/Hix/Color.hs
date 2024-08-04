module Hix.Color where

import Hix.Console (ColorOffsets (..), color, colors)
import Hix.Pretty (HPretty (hpretty))

prettyColor :: HPretty a => Int -> a -> Text
prettyColor offset a =
  color offset (show (hpretty a))

black :: HPretty a => a -> Text
black = prettyColor colors.black

red :: HPretty a => a -> Text
red = prettyColor colors.red

green :: HPretty a => a -> Text
green = prettyColor colors.green

yellow :: HPretty a => a -> Text
yellow = prettyColor colors.yellow

blue :: HPretty a => a -> Text
blue = prettyColor colors.blue

magenta :: HPretty a => a -> Text
magenta = prettyColor colors.magenta

cyan :: HPretty a => a -> Text
cyan = prettyColor colors.cyan

white :: HPretty a => a -> Text
white = prettyColor colors.white

path :: HPretty a => a -> Text
path = blue

shellCommand :: HPretty a => a -> Text
shellCommand = blue

url :: HPretty a => a -> Text
url = blue

package :: HPretty a => a -> Text
package = cyan

number :: HPretty a => a -> Text
number = cyan

config :: HPretty a => a -> Text
config = yellow

env :: HPretty a => a -> Text
env = config
