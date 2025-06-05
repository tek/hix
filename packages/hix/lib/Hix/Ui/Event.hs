-- | Generic event handling for Brick UIs with key dispatchers
module Hix.Ui.Event where

import Brick (BrickEvent (..), EventM)
import Brick.Keybindings (KeyDispatcher, handleKey)
import Graphics.Vty (Event (..))

import Hix.Ui.Debug (BrickDebugEvent (..))

-- | Handle a VTY event using a key dispatcher
vtyEvent ::
  ∀ e n s .
  KeyDispatcher e (EventM n s) ->
  Event ->
  EventM n s ()
vtyEvent dispatcher = \case
  EvKey key mods -> void (handleKey dispatcher key mods)
  _ -> unit

-- | Create a Brick event handler from a key dispatcher
handleEvent ::
  ∀ e n s .
  KeyDispatcher e (EventM n s) ->
  BrickEvent n BrickDebugEvent ->
  EventM n s ()
handleEvent dispatcher = \case
  VtyEvent ve -> vtyEvent dispatcher ve
  AppEvent (BrickVtyEvent ve) -> vtyEvent dispatcher ve
  _ -> unit
