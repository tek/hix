module Hix.Regex where

import Control.Lens.Regex.Text (mkRegexTraversalQQ)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Regex.PCRE.Light (multiline, utf8)

regexMulti :: QuasiQuoter
regexMulti =
  mkRegexTraversalQQ [utf8, multiline]
