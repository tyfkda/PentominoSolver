module Main (main) where
import Control.Monad (forM_)
import Data.Bits (shiftR, (.&.))

import Pentomino (createPentominos)
import Solver (solve)

main :: IO ()
main = do
    let w = 10
    let h = 6
    let board = (w, h, 0)
    let result = solve board $ createPentominos w h
    forM_ (take 10 result) $ \((_, _, bits), _) -> do
        forM_ [0..h - 1] $ \y -> do
            let line = bits `shiftR` (y * w)
            let bin = concat $ take w $ map (show . (.&. 1)) $ iterate (`div` 2) line
            putStrLn bin
        putStrLn ""
