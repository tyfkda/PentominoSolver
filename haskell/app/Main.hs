module Main (main) where
import Control.Monad (forM_)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Pentomino (createPentominos)
import Solver (Board, solve)

solvePentomino :: Board -> IO ()
solvePentomino board@(w, h, _) = do
    let result = solve board $ createPentominos w h
    forM_ (take 10 result) $ \((_, _, bits), _) -> do
        forM_ [0..h - 1] $ \y -> do
            let line = bits `shiftR` (y * w)
            let bin = concat $ take w $ map (show . (.&. 1)) $ iterate (`div` 2) line
            putStrLn bin
        putStrLn ""

data Argument = Argument
    { size :: String
    } deriving (Read, Show)

main :: IO ()
main = do
    args <- execParser parserInfo
    case (boardSize $ size args) of
        Just sz -> solvePentomino sz
        Nothing -> do
            hPutStrLn stderr $ "Illegal size: " ++ size args
            exitFailure
    where
        parserInfo = argumentParser `withInfo` "Pentomino Solver"
        argumentParser = Argument
            <$> strOption (short 's' <> long "size" <> value "6x10" <> help "Board size (6x10, 5x12, 4x15, 3x20, 8x8)")
        withInfo p = info (p <**> helper) . progDesc

boardSize :: String -> Maybe Board
boardSize sizeStr
    | sizeStr == "6x10" || sizeStr == "6"  = Just (10, 6, 0)
    | sizeStr == "5x12" || sizeStr == "5"  = Just (12, 5, 0)
    | sizeStr == "4x15" || sizeStr == "4"  = Just (15, 4, 0)
    | sizeStr == "3x20" || sizeStr == "3"  = Just (20, 3, 0)
    | sizeStr == "8x8"  || sizeStr == "8"  = Just ( 8, 8, (3 `shiftL` (3 * 8 + 3)) .|. (3 `shiftL` (4 * 8 + 3)))
    | otherwise                            = Nothing
