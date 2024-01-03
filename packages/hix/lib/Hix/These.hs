module Hix.These where

import Data.These (These (..))

maybeThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeThese (Just a) (Just b) = Just (These a b)
maybeThese (Just a) Nothing = Just (This a)
maybeThese Nothing (Just b) = Just (That b)
maybeThese Nothing Nothing = Nothing
