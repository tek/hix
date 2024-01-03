module Hix.Maybe where

-- |Return 'Just' if the condition is 'True'.
justIf ::
  Bool ->
  a ->
  Maybe a
justIf cond a =
  if cond then Just a else Nothing
{-# inline justIf #-}

-- |Version of 'maybe' that takes an action as fallback.
fromMaybeA ::
  Applicative m =>
  m a ->
  Maybe a ->
  m a
fromMaybeA fallback =
  maybe fallback pure
{-# inline fromMaybeA #-}
