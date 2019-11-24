{-# OPTIONS_GHC -F -pgmF tasty-discover
                   -optF --num-threads=1
                   -optF --tree-display #-}
-- We force tests to be single-threaded, else the roundtripping test
-- fails, perhaps due to the withSystemTempDirectory; I haven't been
-- able to pin it down.
