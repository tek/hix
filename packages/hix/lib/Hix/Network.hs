module Hix.Network where

import Control.Exception (bracket)
import Network.Socket (
  addrAddress,
  addrFamily,
  addrProtocol,
  addrSocketType,
  bind,
  close,
  defaultHints,
  getAddrInfo,
  socket,
  socketPort,
  withSocketsDo,
  )

newtype Port =
  Port { value :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

freePort ::
  MonadIO m =>
  m Port
freePort =
  liftIO $ withSocketsDo do
    addr : _ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
    bracket (open addr) close (fmap fromIntegral . socketPort)
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      sock <$ bind sock (addrAddress addr)
