module Hix.Ui.KeyMappings where

import Brick (Padding (..), Widget, hLimit, padLeft, txt, vBox, withAttr, (<+>))
import Brick.Keybindings (KeyConfig, KeyEventHandler, firstDefaultBinding, keybindingHelpWidget, ppBinding)
import qualified Data.Text as Text

import Hix.Ui.Data.Attr (helpDescAttr, helpKeyAttr)

data KeyMapping = KeyMapping {
  description :: Text,
  keys :: Text
}

-- | Configuration for the help screen display.
data HelpConfig e m =
  HelpConfig {
    keyConfig :: KeyConfig e,
    helpHandlers :: [KeyEventHandler e m],
    helpEvents :: [(e, Text)]
  }

-- | Render a list of key mappings right-aligned in a column
-- Format:
--        bump b
--      shared s
--   navigate hjkl
--
-- TODO implement navigate?
renderKeyMappings :: [KeyMapping] -> (Int, Widget n)
renderKeyMappings mappings =
  (maxSize, vBox (uncurry renderRow <$> withSize))
  where
    maxSize = fromMaybe 0 (maximum (fst <$> withSize))

    withSize = mappings <&> \ m -> (mappingSize m, m)

    mappingSize KeyMapping {..} = Text.length description + Text.length keys + 1

    renderRow size KeyMapping {description, keys} =
      padLeft (Pad padding) (withAttr helpDescAttr (txt description) <+> txt " " <+> withAttr helpKeyAttr (txt keys))
      where
        padding = maxSize - size

-- | Create a layout with main content and key mappings in the lower right corner.
-- The mappings are pushed to the bottom by padding with empty lines based on content height.
--
-- The content rendering function must ensure that it doesn't include very long lines, otherwise the mappings will be
-- pushed out of the rendering area.
withKeyMappings ::
  -- | Number of lines in the content
  Int ->
  Widget n ->
  [KeyMapping] ->
  Widget n
withKeyMappings contentHeight content mappings =
  content <+> hLimit mappingsSize (vBox (padding ++ [mappingsWidget]))
  where
    (mappingsSize, mappingsWidget) = renderKeyMappings mappings
    mappingsHeight = length mappings
    paddingLines = max 0 (contentHeight - mappingsHeight)
    padding = replicate paddingLines (txt " ")

-- | Convert a Brick 'KeyConfig' to 'KeyMappings' for display.
keyMappingsFromConfig ::
  ∀ e .
  Ord e =>
  Show e =>
  KeyConfig e ->
  -- | Events to display with their descriptions
  [(e, Text)] ->
  [KeyMapping]
keyMappingsFromConfig config events =
  mapMaybe toMapping events
  where
    toMapping :: (e, Text) -> Maybe KeyMapping
    toMapping (event, description) =
      case firstDefaultBinding config event of
        Nothing -> Nothing
        Just binding -> Just KeyMapping {
          description,
          keys = toText (ppBinding binding)
        }

-- | Render either a help screen or the main content with a key mappings corner, based on whether help is currently
-- being shown.
--
-- This standardizes the help screen toggle pattern used across all UI screens.
withHelpScreen ::
  ∀ e n m .
  Ord e =>
  Show e =>
  HelpConfig e m ->
  -- | Whether to show the help screen
  Bool ->
  -- | Number of lines in the content (for positioning mappings)
  Int ->
  -- | Main content widget
  Widget n ->
  Widget n
withHelpScreen HelpConfig {..} showingHelp contentHeight content
  | showingHelp = keybindingHelpWidget keyConfig helpHandlers
  | otherwise = withKeyMappings contentHeight content (keyMappingsFromConfig keyConfig helpEvents)
