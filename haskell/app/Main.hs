module Main (main) where
import Control.Monad (forM_, void)
import Data.Bits (shiftL, (.|.))
import Data.List (unfoldr)
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

choosePrinter :: Bool -> ((Char, Char) -> IO (), IO ())
choosePrinter True  = ( \(c, c') -> colorStr (chooseColor c) [c', ' ']
                      , void resetColor )
choosePrinter False = ( \(c, _) -> putChar c
                      , return () )

printSolution :: ((Char, Char) -> IO (), IO ()) -> Int -> Int -> Solution -> IO ()
printSolution (putc, eol) w _ sol = do
    let ls = splitByWidth w resultList
    forM_ ls $ \ll -> do
        forM_ ll $ \cc -> do
            putc cc
        eol >> putStrLn ""
    where
        resultList = unfoldr f (sol, "")
        f (c:cs, used) | c `elem` used = Just ((c, ' '), (cs, used))
                       | otherwise     = Just ((c, c),   (cs, c:used))
        f ([], _)      = Nothing

solvePentomino :: Board -> IO ()
solvePentomino board@(w, h, _) = do
    let result = solve board $ createPentominos w h
    istty <- queryTerminal stdOutput
    let printer = choosePrinter istty
    forM_ result $ \sol -> do
        printSolution printer w h sol
        putStrLn ""
    print $ length result

newtype Argument = Argument
    { size :: String
    } deriving (Read, Show)

main :: IO ()
main = do
    args <- execParser parserInfo
    case boardSize $ size args of
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
