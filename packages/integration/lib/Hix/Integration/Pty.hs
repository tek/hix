module Hix.Integration.Pty where

import Control.Concurrent (threadDelay)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadMask (..), bracket)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Extra (loopM)
import Control.Monad.Morph (hoist)
import qualified Control.Monad.Reader as MTL
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.List (dropWhileEnd)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (TestT (..))
import Path (Abs, File, Path, relfile, toFilePath, (</>))
import Path.IO (doesFileExist)
import System.IO (Handle)
import System.IO.Error (tryIOError)
import System.Posix (
  Fd,
  OpenMode (..),
  closeFd,
  createNamedPipe,
  defaultFileFlags,
  fdToHandle,
  openFd,
  openPseudoTerminal,
  sigKILL,
  signalProcess,
  )
import System.Posix.ByteString (fdRead)
import System.Posix.Pty (Pty, closePty, createPty, resizePty)
import System.Process.Typed (
  ProcessConfig,
  StreamSpec,
  getPid,
  proc,
  readProcessInterleaved_,
  runProcess,
  setCloseFds,
  setStderr,
  setStdin,
  setStdout,
  startProcess,
  stopProcess,
  useHandleClose,
  )
import System.Timeout (timeout)

import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Error (pathText)
import qualified Hix.Handlers.Tui.Test as Tui
import qualified Hix.Log as Log
import Hix.Monad (appContextVerbose, appContextVerboseIO, fatalError, noteFatal, tryIOErrorLog, tryIOM)
import Hix.Monad.Run (runWithTui)
import Hix.Test.Hedgehog (TestTCompat (..), eqLines, listEqTail, runTestTCompat)
import Hix.Test.Utils (runTestTM)

data PtyResources =
  PtyResources {
    primary :: Fd,
    secondary :: Fd,
    handle :: Handle,
    pty :: Pty
  }

acquirePty :: M PtyResources
acquirePty = do
  noteFatal "no pty returned" =<< appContextVerboseIO "creating pseudo terminal" do
    (primary, secondary) <- openPseudoTerminal
    handle <- fdToHandle secondary
    createPty secondary <&> fmap \ pty -> PtyResources {..}

releasePty :: PtyResources -> M ()
releasePty PtyResources {primary, pty} =
  liftIO do
    void $ tryIOError $ closePty pty
    void $ tryIOError $ closeFd primary

withPty ::
  MonadTrans t =>
  MonadMask (t M) =>
  Int ->
  Int ->
  (PtyResources -> t M a) ->
  t M a
withPty width height use =
  bracket (lift acquirePty) (lift . releasePty) \ res -> do
    lift $ appContextVerboseIO "resizing pty" do
      resizePty res.pty (width, height)
    use res

withPtyRead ::
  MonadTrans t =>
  MonadMask (t M) =>
  MonadIO (t M) =>
  Int ->
  Int ->
  (PtyResources -> t M a) ->
  t M ([Text], a)
withPtyRead width height use =
  withPty width height \ res -> do
    result <- use res
    content <- Text.lines . decodeUtf8 <$> liftIO (fdRead res.primary 100000)
    pure (content, result)

bashrcContent :: [Text]
bashrcContent =
  [
    "PS1='$ '"
  ]

createTmuxConf ::
  MonadTrans t =>
  MonadIO (t M) =>
  Path Abs File ->
  [Text] ->
  t M (Path Abs File)
createTmuxConf wait content = do
  tmp <- lift appRes.tmp
  let bashrc = tmp </> [relfile|bashrc|]
      tmuxConf = tmp </> [relfile|tmux.conf|]
  liftIO do
    Text.writeFile (toFilePath bashrc) (Text.unlines bashrcContent)
    Text.writeFile (toFilePath tmuxConf) (Text.unlines (defaultContent bashrc ++ content ++ initCommands))
  pure tmuxConf
  where
    defaultContent rc =
      [
        [exon|set -g default-command '/usr/bin/env bash --noprofile --rcfile #{pathText rc}'|],
        "set -g status off"
      ]

    initCommands =
      [
        [exon|run-shell -b 'touch #{pathText wait}'|]
      ]

testTmuxProcessConfig ::
  MonadTrans t =>
  MonadIO (t M) =>
  PtyResources ->
  Path Abs File ->
  Path Abs File ->
  t M (ProcessConfig () () ())
testTmuxProcessConfig PtyResources {..} wait socket = do
  confFile <- createTmuxConf wait []
  let
    tmuxArgs =
      ["-S", pathText socket, "-f", pathText confFile]
    prc = proc "tmux" (toString <$> tmuxArgs)
  pure (setCloseFds True (stdio (useHandleClose handle) prc))
  where
    stdio (s :: ∀ st . StreamSpec st ()) =
      setStdin s . setStdout s . setStderr s

waitForFile ::
  Path Abs File ->
  M ()
waitForFile file =
  flip loopM (0 :: Word) \ count -> do
    if count >= 300
    then fatalError [exon|tmux wait file '#{pathText file}' didn't appear within 3 seconds|]
    else do
      exists <- doesFileExist file
      if exists
      then pure (Right ())
      else Left (count + 1) <$ liftIO (threadDelay 10_000)

withTmux ::
  MonadTrans t =>
  MonadMask (t M) =>
  MonadIO (t M) =>
  PtyResources ->
  (Path Abs File -> t M a) ->
  t M a
withTmux res use = do
  tmp <- lift appRes.tmp
  let wait = tmp </> [relfile|tmux-wait|]
      socket = tmp </> [relfile|tmux-socket|]
  bracket (acquire wait socket) (lift . release socket) \ _ -> do
    lift $ waitForFile wait
    use socket
  where
    acquire wait socket = do
      processConfig <- testTmuxProcessConfig res wait socket
      lift do
        p <- appContextVerboseIO "starting tmux process" do
          timeout 1_000_000 (startProcess processConfig)
        noteFatal "tmux didn't start" p

    release socket p = do
      appContextVerbose "sending kill-server" do
        tryIOErrorLog do
          runProcess (proc "tmux" (["-S", toFilePath socket, "kill-server"]))
      -- The tmux client process catches SIGTERM and doesn't exit because it's blocked on pty I/O.
      -- Sending SIGKILL ensures it terminates immediately.
      appContextVerbose "stopping tmux process" do
        tryIOErrorLog do
          liftIO (getPid p) >>= traverse_ \ pid ->
            signalProcess sigKILL pid
          stopProcess p

data Tmux =
  Tmux {
    run :: [Text] -> M Text,
    hIn :: Handle,
    hOut :: Handle,
    fdIn :: Fd,
    fdOut :: Fd
  }

newtype TmuxTest m a =
  TmuxTest { test :: TestT (ReaderT Tmux m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

hoistTmuxTest ::
  (forall x . M x -> M x) ->
  TmuxTest M a ->
  TmuxTest M a
hoistTmuxTest f (TmuxTest ma) = TmuxTest (hoist (hoist f) ma)

deriving newtype instance MonadBase IO m => MonadBase IO (TmuxTest m)

deriving newtype instance MonadBaseControl IO m => MonadBaseControl IO (TmuxTest m)

deriving newtype instance MonadError e m => MonadError e (TmuxTest m)

instance MonadReader AppResources (TmuxTest M) where
  ask = TmuxTest (lift (lift MTL.ask))
  local f = hoistTmuxTest (MTL.local f)

getTmux ::
  Monad m =>
  TmuxTest m Tmux
getTmux = do
  TmuxTest (lift ask)

tmuxLiftM :: M a -> TmuxTest M a
tmuxLiftM =
  TmuxTest . lift . lift

withTmuxTui ::
  PtyResources ->
  TmuxTest M a ->
  TestTCompat M a
withTmuxTui res (TmuxTest test) =
  withTmux res \ socket -> do
    tmp <- lift appRes.tmp
    let
      inputFifo = tmp </> [relfile|tmux-input|]
      outputFifo = tmp </> [relfile|tmux-output|]
      inputFp = toFilePath inputFifo
      outputFp = toFilePath outputFifo
      runTmux args = decodeUtf8 <$> tryIOM (readProcessInterleaved_ (proc "tmux" (["-S", toFilePath socket] ++ args)))
    lift do
      appContextVerboseIO "creating named pipes" do
        createNamedPipe inputFp 0o600
        createNamedPipe outputFp 0o600
      appContextVerbose "setting up pipes in tmux" do
        void $ runTmux ["send-keys", "stty -icanon -echo", "ENTER"]
        void $ runTmux ["send-keys", [exon|cat #{inputFp} & cat - > #{outputFp}|], "ENTER"]
    bracket (liftIO (openFd inputFp WriteOnly defaultFileFlags)) (lift . tryIOErrorLog . closeFd) \ fdOut ->
      bracket (liftIO (openFd outputFp ReadOnly defaultFileFlags)) (lift . tryIOErrorLog . closeFd) \ fdIn -> do
        hIn <- liftIO $ fdToHandle fdIn
        hOut <- liftIO $ fdToHandle fdOut
        let tmux = Tmux {run = runTmux . fmap toString, hIn, hOut, fdIn, fdOut}
        lift $ Log.debug "running tmux test"
        TestTCompat (hoist (flip runReaderT tmux) test)

withTmuxPtyTui ::
  Int ->
  TmuxTest M a ->
  TestT M a
withTmuxPtyTui height test =
  runTestTCompat $ withPty 100 height \ res ->
    withTmuxTui res do
      Tmux {..} <- getTmux
      tuiHandlers <- liftIO (Tui.handlersTestTmux fdIn fdOut hIn hOut)
      hoistTmuxTest (runWithTui tuiHandlers) test

tmuxCmd ::
  [Text] ->
  TmuxTest M Text
tmuxCmd cmd = do
  Tmux {run} <- getTmux
  tmuxLiftM $ run cmd

tmuxTest ::
  Bool ->
  Int ->
  TmuxTest M a ->
  TestT IO a
tmuxTest debug height test =
  runTestTM debug (withTmuxPtyTui height test)

capture ::
  TmuxTest M Text
capture =
  tmuxCmd ["capture-pane", "-p", "-e"]

showTmux ::
  TmuxTest M ()
showTmux = do
  output <- capture
  liftIO do
    Text.putStrLn "-----------------------------------"
    -- trailing newline
    Text.putStrLn (Text.dropEnd 1 output)
    Text.putStrLn "-----------------------------------"

capturePlain ::
  TmuxTest M Text
capturePlain =
  tmuxCmd ["capture-pane", "-p"]

assertTmux ::
  Bool ->
  Text ->
  TmuxTest M ()
assertTmux escapeSequences target = do
  actual <- if escapeSequences then capture else capturePlain
  TmuxTest $ eqLines (Text.stripEnd target) (Text.stripEnd actual)

assertTmuxTail ::
  Bool ->
  Text ->
  TmuxTest M ()
assertTmuxTail escapeSequences target = do
  actual <- if escapeSequences then capture else capturePlain
  TmuxTest $ listEqTail (Text.lines (Text.stripEnd target)) (dropWhileEnd Text.null (Text.lines actual))
