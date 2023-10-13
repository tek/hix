{-# language CPP #-}

module Root.LibGhc where

message :: String
#if MIN_VERSION_ghc(9,6,0)
message = "over"
#else
message = "under"
#endif

main :: IO ()
main = putStrLn message
