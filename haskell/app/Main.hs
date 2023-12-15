module Main (main) where
import Control.Monad (forM_, unless)
import Data.Array (Array, array, (!))
import Data.Bits (shiftL, (.|.))
import Data.Char (isAlpha)
import Data.List (scanl', transpose, unfoldr)
import Data.Time (diffUTCTime, getCurrentTime)
import Options.Applicative
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Pentomino (createPentominos)
import Solver (Board, Solution, solve)
import Util (splitByWidth)

type ColorPair = (ColorIntensity, Color)

gridChars :: Array Int Char
gridChars = array (0, 15) [
    (3, '━'), (5, '┏'), (6, '┓'), (7, '┳'),
    (9, '┗'), (10, '┛'), (11, '┻'),
    (12, '┃'), (13, '┣'), (14, '┫'), (15, '╋'),
    (0, ' ')]  --, (1, ' '), (2, ' '), (4, ' '), (8, ' ')]

printSolutionFigure :: Int -> Int -> [Solution] -> IO ()
printSolutionFigure w h result = do
    forM_ result $ \sol -> do
        printSolution sol
    where
        printSolution sol = do
            forM_ scannedGrid $ \line -> do
                putStrLn line
            putStrLn ""
            where
                scannedGrid = fst $ scann (flip scanLine) ("", []) grid2
                scanLine ls = scann elimDupAlpha (' ', ls)
                elimDupAlpha c ls | c `elem` ls  = (' ', ls)
                                  | otherwise    = (c, if isAlpha c then c : ls else ls)
                scann f ini line = (map fst res, snd $ last res)
                    where res = tail $ scanl' (\(_c, ls) c -> f c ls) ini line

                placedTranspose = splitByWidth w sol
                placed = transpose placedTranspose
                hlines = zipWith (zipWith diff) placed (tail placed)
                vlines = transpose $ zipWith (zipWith diff) placedTranspose (tail placedTranspose)
                hlines2' = (replicate h 1 : hlines) ++ [replicate h 1]
                hlines2 = map (\hh -> zipWith (\h1 h2 -> h1 * 2 + h2) (0: hh) (hh ++ [0])) hlines2'
                vlines2' = map (\vv -> (1 : vv) ++ [1]) vlines
                vlines2 = zipWith (zipWith (\v1 v2 -> v1 * 2 + v2)) (replicate (h + 1) 0 : vlines2') (vlines2' ++ [replicate (h + 1) 0])
                grid = zipWith (zipWith (\ih iv -> ih + iv * 4)) hlines2 vlines2
                grid2 = zipWith3 (\l1 l2 l3 -> concat $ transpose [map (gridChars !) l1, zipWith (\i c -> if i /= 0 then gridChars ! (i * 3) else c) l2 l3]) grid hlines2' (replicate h ' ' : placed)
        diff a b | a == b  = 0
                 | otherwise = 1


resetColor :: IO ()
resetColor = do
    setSGR []

setColor :: ColorPair -> ColorPair -> IO ()
setColor (fgi, fg) (bgi, bg) = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]

colorStr :: (ColorPair, ColorPair) -> String -> IO ()
colorStr (fc, bc) ss = do
    setColor fc bc
    putStr ss

vividWhite :: ColorPair
vividWhite = (Vivid, White)
dullBlack :: ColorPair
dullBlack = (Dull, Black)

chooseColor :: Char -> (ColorPair, ColorPair)
chooseColor 'F' = (dullBlack, (Vivid, Red))
chooseColor 'I' = (vividWhite, (Dull, Magenta))
chooseColor 'L' = (dullBlack, (Vivid, Yellow))
chooseColor 'N' = (vividWhite, (Vivid, Blue))
chooseColor 'P' = (dullBlack, (Vivid, Magenta))
chooseColor 'T' = (dullBlack, (Vivid, Cyan))
chooseColor 'U' = (vividWhite, (Dull, Red))
chooseColor 'V' = (vividWhite, (Dull, Blue))
chooseColor 'W' = (vividWhite, (Dull, Yellow))
chooseColor 'X' = (dullBlack, (Vivid, Green))
chooseColor 'Y' = (vividWhite, (Dull, Green))
chooseColor 'Z' = (vividWhite, (Dull, Cyan))
chooseColor _   = (dullBlack, vividWhite)

printSolutionColor :: Int -> Int -> [Solution] -> IO ()
printSolutionColor w _ result = do
    forM_ result $ \sol -> do
        -- Transpose board to reduce lines.
        let ls = transpose $ splitByWidth w $ resultList sol
        forM_ ls $ \ll -> do
            forM_ ll $ \(c, c') -> do
                colorStr (chooseColor c) [c', ' ']
            resetColor >> putStrLn ""
        putStrLn ""
        where
            resultList sol = unfoldr f (sol, "")
            f (c:cs, used) | c `elem` used = Just ((c, ' '), (cs, used))
                        | otherwise     = Just ((c, c),   (cs, c:used))
            f ([], _)      = Nothing

solvePentomino :: Board -> (Bool, Bool) -> IO ()
solvePentomino board@(w, h, _) (isQuiet, isFigure) = do
    start <- getCurrentTime
    let result = solve board $ createPentominos w h
    unless isQuiet $ do
        istty <- queryTerminal stdOutput
        printSolution (not isFigure && istty) w h result
    print $ length result
    stop <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime stop start)
    where
        printSolution False = printSolutionFigure
        printSolution True  = printSolutionColor

data Argument = Argument
    { size :: String
    , figure :: Bool
    , quiet :: Bool
    } deriving (Read, Show)

main :: IO ()
main = do
    args <- execParser parserInfo
    case boardSize $ size args of
        Just board -> solvePentomino board (quiet args, figure args)
        Nothing -> do
            hPutStrLn stderr $ "Illegal size: " ++ size args
            exitFailure
    where
        parserInfo = argumentParser `withInfo` "Pentomino Solver"
        argumentParser = Argument
            <$> strOption (short 's' <> long "size" <> value "6x10" <> help "Board size (6x10, 5x12, 4x15, 3x20, 8x8)")
            <*> switch (long "figure" <> help "Print as figure")
            <*> switch (short 'q' <> long "quiet" <> help "Suppress solution output")
        withInfo p = info (p <**> helper) . progDesc

boardSize :: String -> Maybe Board
boardSize sizeStr
    | sizeStr == "6x10" || sizeStr == "6" || sizeStr == "10x6"  = Just (6, 10, 0)
    | sizeStr == "5x12" || sizeStr == "5" || sizeStr == "12x5"  = Just (5, 12, 0)
    | sizeStr == "4x15" || sizeStr == "4" || sizeStr == "15x4"  = Just (4, 15, 0)
    | sizeStr == "3x20" || sizeStr == "3" || sizeStr == "20x3"  = Just (3, 20, 0)
    | sizeStr == "8x8"  || sizeStr == "8"  = Just ( 8, 8, (3 `shiftL` (3 * 8 + 3)) .|. (3 `shiftL` (4 * 8 + 3)))
    | otherwise                            = Nothing
