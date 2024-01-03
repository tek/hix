module Hix.Zip where

zipApply :: (a -> b) -> [a] -> [(a, b)]
zipApply f = fmap \ a -> (a, f a)

zipApplyA ::
  Traversable t =>
  Applicative m =>
  (a -> m b) ->
  t a ->
  m (t (a, b))
zipApplyA f = traverse \ a -> (a,) <$> f a

zipApplyL :: (a -> b) -> [a] -> [(b, a)]
zipApplyL f = fmap \ a -> (f a, a)
