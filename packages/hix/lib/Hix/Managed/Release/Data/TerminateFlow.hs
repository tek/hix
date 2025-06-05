module Hix.Managed.Release.Data.TerminateFlow where

-- | Reason for terminating the release flow early.
-- When a handler returns @'Left' TerminateFlow@, the flow transitions to the 'Terminated' stage,
-- preventing further stage transitions and preserving the reason for reporting.
newtype TerminateFlow =
  TerminateFlow { reason :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)
