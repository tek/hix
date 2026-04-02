module Hix.Terminal.Vty where

import Blaze.ByteString.Builder (Write)
import Blaze.ByteString.Builder.Internal.Write (Poke (..), boundedWrite, getBound, runWrite)
import Control.Lens ((.~))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.IORef.Extra (atomicModifyIORef'_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Graphics.Vty (DisplayContext (..), Image, Output (..), Picture (..), Vty (..), imageHeight)
import Graphics.Vty.Image.Internal (Image (..))
import System.Console.ANSI (hSetCursorPosition)
import System.IO (Handle, hFlush)

import Hix.Terminal.State (OutputState (..), fromState)

newtype GetTerminalBounds =
  GetTerminalBounds (IO (Int, Int))

displayBoundsWithRenderArea ::
  IORef OutputState ->
  GetTerminalBounds ->
  IO (Int, Int)
displayBoundsWithRenderArea state (GetTerminalBounds terminalBounds) = do
  (w, h) <- terminalBounds
  height <- fromState state \ OutputState {renderHeight} -> renderHeight
  let g = (w, fromMaybe h height)
  pure g

writeMoveCursorAdjusted ::
  IORef OutputState ->
  DisplayContext ->
  Int ->
  Int ->
  Write
writeMoveCursorAdjusted state base x y =
  boundedWrite (getBound (base.writeMoveCursor 0 0)) $ Poke \ pf -> do
    OutputState {firstLine} <- readIORef state
    let adjustedY = firstLine + y
    runWrite (base.writeMoveCursor x adjustedY) pf

customizeVtyOutput ::
  IORef OutputState ->
  Output ->
  Bool ->
  IO Output
customizeVtyOutput state output nativeBounds = do
  pure output {
    releaseTerminal = unit,
    reserveDisplay = unit,
    releaseDisplay = unit,
    setMode = \ _ _ -> unit,
    setDisplayBounds = const unit,
    displayBounds = displayBoundsWithRenderArea state (GetTerminalBounds terminalBounds),
    mkDisplayContext = \ o reg -> do
      base <- output.mkDisplayContext o reg
      pure base {writeMoveCursor = writeMoveCursorAdjusted state base}
  }
  where
    terminalBounds =
      if nativeBounds
      then output.displayBounds
      else do
        fromState state (.geometry)

ensureSpace ::
  Handle ->
  IORef OutputState ->
  Output ->
  Int ->
  IO ()
ensureSpace hOut stateRef output pictureHeight = do
  (_, termHeight) <- output.displayBounds
  OutputState {firstLine, ..} <- readIORef stateRef
  let avail = termHeight - firstLine
      excess = pictureHeight - avail
  when (excess > 0) do
    let newFirstLine = firstLine - excess
    hSetCursorPosition hOut (termHeight - 1) 0
    Text.hPutStr hOut (Text.replicate excess "\n")
    hFlush hOut
    writeIORef stateRef OutputState {firstLine = newFirstLine, ..}

-- | Set 'regionHeight' to the height of the rendering area below the cursor while executing the given action.
-- When called in 'Vty.update', this has the effect that 'displayBounds' returns the (usually) smaller height only while
-- rendering the picture.
--
-- The reason for this is the following:
-- 1. We want the rendering area to be below the cursor, using exactly as much of the screen as needed.
--    We can't predict the UI size, so we have to render the Brick UI to a 'Picture' using the full terminal height and
--    measure its effective size to determine the required space.
-- 2. Brick uses 'displayBounds' to fit its widgets into the rendering area, so that function must return the terminal
--    size while Brick is rendering the 'Picture'.
--    Brick also fills the remaining vertical space with empty lines.
-- 3. 'Vty.update' uses 'displayBounds' to fill the remaining vertical space _again_ with empty lines.
-- 4. When 'Vty.update' is executed for the resulting 'Picture', we can measure its height.
--    The padding added by Brick is stripped by 'truncatePicture', yielding the height of the content.
-- 5. The UI can change in size over the course of its lifetime.
--    If a 'Picture' is smaller than the previous iteration's, using the new height as the rendering area would leave
--    the excess lines intact, so we take the maximum of the new height and the available space as the rendering area,
--    resulting in 'Vty.update' erasing the excess lines.
-- 6. 'Output' is part of the closure of 'Vty.update' as well as exposed to Brick via 'Vty.outputIface', so we can't
--    get around the fact that they both call the same 'displayBounds'.
-- 7. To prevent 'Vty.update' from filling the remaining space, we set the height returned by 'displayBounds' to the
--    height of the rendering area while it is executing.
--
-- TODO This could just call output.displayBounds instead of using the geometry from state, since that's what's returned
-- from there anyway in test mode, and in native we can benefit from more recent data.
withRenderHeight ::
  IORef OutputState ->
  Int ->
  IO a ->
  IO a
withRenderHeight stateRef pictureHeight ma = do
  atomicModifyIORef'_ stateRef \ s@OutputState {geometry = (_, termHeight), firstLine} ->
    let renderHeight = max (termHeight - firstLine) pictureHeight
    in s {renderHeight = Just renderHeight}
  ma <* atomicModifyIORef'_ stateRef (#renderHeight .~ Nothing)

stripFill :: Image -> Image
stripFill = \case
  VertJoin {partTop, partBottom = BGFill {}} -> partTop
  i -> i

truncatePicture :: Picture -> Picture
truncatePicture pic =
  pic {picLayers = stripFill <$> pic.picLayers}

updateTruncated ::
  Handle ->
  IORef OutputState ->
  Vty ->
  Picture ->
  IO ()
updateTruncated hOut state vty picture = do
  ensureSpace hOut state vty.outputIface picHeight
  withRenderHeight state picHeight do
    vty.update truncated
  atomicModifyIORef'_ state (#lastRenderHeight .~ picHeight)
  where
    picHeight = fromMaybe 1 (maximum (imageHeight <$> truncated.picLayers))

    truncated = truncatePicture picture
