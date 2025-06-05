module Hix.Handlers.Tui.Prod where

import Brick (App (..), customMain)
import Brick.BChan (BChan)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Graphics.Vty as Vty
import Graphics.Vty (Input, Output, defaultConfig, mkVtyFromPair)
import Graphics.Vty.Platform.Unix.Input (buildInput)
import Graphics.Vty.Platform.Unix.Output (buildOutput)
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (..), defaultSettings)
import System.IO (Handle, stdin, stdout)
import System.Posix (Fd, stdInput, stdOutput)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Handlers.Tui (RunBrick (..), TuiHandlers (..))
import Hix.Terminal.State (VtyTuiResources (..), bracketTerminal)
import Hix.Terminal.Vty (customizeVtyOutput, updateTruncated)

runBrickWith ::
  Ord n =>
  Bool ->
  Maybe String ->
  (UnixSettings -> IO (Input, Output)) ->
  IORef VtyTuiResources ->
  App s e n ->
  Maybe (BChan e) ->
  s ->
  M s
runBrickWith nativeBounds forceTerm mkIO tuiState app events appState = do
  VtyTuiResources {fdIn, fdOut, hOut} <- liftIO $ readIORef tuiState
  bracketTerminal tuiState \ state -> do
    settingsBase <- defaultSettings
    let settings = settingsBase {
      settingInputFd = fdIn,
      settingOutputFd = fdOut,
      settingTermName = fromMaybe settingsBase.settingTermName forceTerm
    }
    let
      newVty = do
        (input, output0) <- mkIO settings
        output <- customizeVtyOutput state output0 nativeBounds
        vtyBase <- mkVtyFromPair input output
        pure vtyBase {Vty.update = updateTruncated hOut state vtyBase}
    initialVty <- newVty
    customMain initialVty newVty events app appState

runBrickNative ::
  Ord n =>
  IORef VtyTuiResources ->
  App s e n ->
  Maybe (BChan e) ->
  s ->
  M s
runBrickNative =
  runBrickWith True Nothing \ settings -> do
    input <- buildInput defaultConfig settings
    output <- buildOutput defaultConfig settings
    pure (input, output)

handlersProdWith ::
  Fd ->
  Fd ->
  Handle ->
  Handle ->
  IO (TuiHandlers M)
handlersProdWith fdIn fdOut hIn hOut = do
  state <- newIORef VtyTuiResources {fdIn, fdOut, hIn, hOut}
  pure TuiHandlers {
    log = \ _ -> Console.err,
    runBrick = RunBrick (runBrickNative state)
  }

handlersProd :: IO (TuiHandlers M)
handlersProd = handlersProdWith stdInput stdOutput stdin stdout
