module Hix.Data.Error where

data Error =
  PreprocError Text
  |
  EnvError Text
  |
  GhciError Text
  |
  NewError Text
  |
  BootstrapError Text
  |
  NoMatch Text
  |
  Fatal Text
  |
  Client Text
  deriving stock (Eq, Show, Generic)
