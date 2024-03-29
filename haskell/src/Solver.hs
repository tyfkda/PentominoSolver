module Solver
    ( Board, Pos, Solution
    , solve
    ) where
import Control.Monad (forM_)
import Data.Array.ST (MArray(newArray), runSTArray, writeArray)
import Data.Bits (countTrailingZeros, shiftL, Bits ((.&.), (.|.)))
import Data.Either (lefts)
import Data.Foldable (toList)
import qualified Data.HashSet as HS
import Data.List (foldl', partition, transpose, unfoldr)
import Data.Maybe (catMaybes, mapMaybe)

import Pentomino (BitBoard, Piece, Shape, shapeCells)
import Util (eachElem, splitByWidth, unfoldTree)

type Pos = (Int, Int)
type Board = (Int, Int, BitBoard)  -- w, h, bitpat
type PieceArrange = (Char, Pos, Shape)
type Solution = [Char]
type Node = ([Piece], Pos, Board, [PieceArrange])

solve :: Board -> [Piece] -> [Solution]
solve board@(w, h, _) pieces = removeRedundant [toSolution w h arranges | arranges <- arrangess]
    where
        arrangess = case partition (\(c, _) -> c == 'X') pieces of
            ([x], ps) -> solveXFirst board x ps
            _         -> solveRecur [(pieces, (0, 0), board, [])]
        removeRedundant = catMaybes . unfoldr f . (,) HS.empty
        f (_, [])                            = Nothing
        f (hs, sol: sols) | HS.member sol hs = Just (Nothing, (hs, sols))
                          | otherwise        = Just (Just sol, (registerSolution w h sol hs, sols))

toSolution :: Int -> Int -> [PieceArrange] -> Solution
toSolution w h arranges = toList resultArray
    where
        resultArray = runSTArray $ do
            arr <- newArray (0, w * h - 1) '.'
            forM_ arranges $ \(c, (x, y), shape) -> do
                forM_ (shapeCells shape) $ \ofs -> do
                    writeArray arr (y * w + x + ofs) c
            return arr

registerSolution :: Int -> Int -> Solution -> HS.HashSet Solution -> HS.HashSet Solution
registerSolution w h sol hs = foldl' (flip HS.insert) hs $ [concat ss | ss <- mirrors ++ rotated]
    where
        mirrors = [mx, my, mxy]
        rotated = if w /= h then [] else [rot90 bss, rot90 mx, rot90 my, rot90 mxy]
        mx = map reverse bss
        my = reverse bss
        mxy = map reverse $ reverse bss
        rot90 = transpose . reverse
        bss = splitByWidth w sol

solveXFirst :: Board -> Piece -> [Piece] -> [[PieceArrange]]
solveXFirst board@(w, h, _) xp@(_, shapes) pieces = solveRecur xplaced
    where
        xplaced = [(pieces, (0, 0), b, pa) | (_, _, b, pa) <- xnodes]
        (_, sw, sh, ofsx) = head $ shapes
        range = [(x, y) | y <- [0..(h - sh) `div` 2], x <- [ofsx..(w - sw) `div` 2 + ofsx]]
        xnodes = concat $ lefts [expandNode ([xp], pos, board, []) | pos <- range]

solveRecur :: [Node] -> [[PieceArrange]]
solveRecur = unfoldTree expandNode . Left

expandNode :: Node -> Either [Node] [PieceArrange]
expandNode ([], _, _, sols)                  = Right sols
expandNode (pieces, pos@(x, y), board, sols) = Left oneStep
    where
        oneStep = concatMap f $ eachElem pieces
        f (p, ps) = map (g p ps) $ putPiece pos board p
        g (c, _) ps (s'@(_, _, _, ofsx), b') = (ps, nextPos b', b', (c, (x - ofsx, y), s'): sols)

putPiece :: Pos -> Board -> Piece -> [(Shape, Board)]
putPiece pos board (_, shapes) = mapMaybe f shapes
    where
        f shape = (,) shape <$> putShape pos board shape

putShape :: Pos -> Board -> Shape -> Maybe Board
putShape (x, y) (bw, bh, boardbits) (shapebits, sw, sh, ofsx)
    | inRange && noConflict  = Just (bw, bh, boardbits')
    | otherwise              = Nothing
    where
        inRange = y + sh <= bh && x - ofsx >= 0 && x - ofsx + sw <= bw
        noConflict = (boardbits .&. (shapebits `shiftL` (y * bw + x - ofsx))) == 0
        boardbits' = boardbits .|. (shapebits `shiftL` (y * bw + x - ofsx))

nextPos :: Board -> Pos
nextPos (bw, _, bits) = (index `mod` bw, index `div` bw)
    where
        index = countTrailingOnes bits
        countTrailingOnes = countTrailingZeros . (+ 1)
