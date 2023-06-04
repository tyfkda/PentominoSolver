module Solver
    ( Board, Pos, Solution
    , solve
    ) where
import Control.Monad (forM_)
import Data.Array.ST (MArray(newArray), runSTArray, writeArray)
import Data.Bits (shiftL, Bits ((.&.), (.|.)))
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
        removeRedundant = catMaybes . unfoldr f . ((,) HS.empty)
        f (_, [])                            = Nothing
        f (hs, sol: sols) | HS.member sol hs = Just (Nothing, (hs, sols))
                          | otherwise        = Just (Just sol, (registerSolution w h sol hs, sols))

toSolution :: Int -> Int -> [PieceArrange] -> Solution
toSolution w h arranges = toList resultArray
    where
        resultArray = runSTArray $ do
            arr <- newArray (0, w * h - 1) '.'
            forM_ arranges $ \(c, (x, y), shape) -> do
                forM_ (shapeCells w h shape) $ \(dx, dy) -> do
                    writeArray arr ((y + dy) * w + (x + dx)) c
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
        (_, sw, sh, ofsy) = head $ shapes
        range = [(x, y) | x <- [0..(w - sw) `div` 2], y <- [ofsy..(h - sh) `div` 2 + ofsy]]
        xnodes = concat $ lefts [expandNode ([xp], pos, board, []) | pos <- range]

solveRecur :: [Node] -> [[PieceArrange]]
solveRecur = unfoldTree expandNode . Left

expandNode :: Node -> Either [Node] [PieceArrange]
expandNode ([], _, _, sols)                  = Right sols
expandNode (pieces, pos@(x, y), board, sols) = Left oneStep
    where
        oneStep = concatMap f $ eachElem pieces
        f (p, ps) = map (g p ps) $ putPiece pos board p
        g (c, _) ps (s'@(_, _, _, ofsy), b') = (ps, nextPos b' pos, b', (c, (x, y - ofsy), s'): sols)

putPiece :: Pos -> Board -> Piece -> [(Shape, Board)]
putPiece pos board (_, shapes) = mapMaybe f shapes
    where
        f shape = ((,) shape) <$> putShape pos board shape

putShape :: Pos -> Board -> Shape -> Maybe Board
putShape (x, y) (bw, bh, boardbits) (shapebits, sw, sh, ofsy)
    | inRange && noConflict  = Just (bw, bh, boardbits')
    | otherwise              = Nothing
    where
        inRange = x + sw <= bw && y - ofsy >= 0 && y - ofsy + sh <= bh
        noConflict = (boardbits .&. (shapebits `shiftL` ((y - ofsy) * bw + x))) == 0
        boardbits' = boardbits .|. (shapebits `shiftL` ((y - ofsy) * bw + x))

nextPos :: Board -> Pos -> Pos
nextPos (bw, bh, bits) = head . dropWhile occupied . iterate next
    where
        next (x, y) | y + 1 < bh  = (x, y + 1)
                    | otherwise   = (x + 1, 0)
        occupied (x, y) = x < bw && y < bh && bits .&. (1 `shiftL` (y * bw + x)) /= 0
