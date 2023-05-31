module Solver
    ( Board, Pos, Solution
    , solve
    ) where
import Control.Monad (forM_)
import Data.Array.ST (MArray(newArray), runSTArray, writeArray)
import Data.Bits (shiftL, Bits ((.&.), (.|.)))
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

import Pentomino (BitBoard, Piece, Shape, shapeCells)
import Util (eachElem, unfoldTree)

type Pos = (Int, Int)
type Board = (Int, Int, BitBoard)  -- w, h, bitpat
type PieceArrange = (Char, Pos, Shape)
type Solution = [Char]
type Node = ([Piece], Pos, Board, [PieceArrange])

solve :: Board -> [Piece] -> [Solution]
solve board@(w, h, _) pieces = [toSolution w h arranges | arranges <- solveRecur [(pieces, (0, 0), board, [])]]

toSolution :: Int -> Int -> [PieceArrange] -> [Char]
toSolution w h arranges = toList resultArray
    where
        resultArray = runSTArray $ do
            arr <- newArray (0, w * h - 1) '.'
            forM_ arranges $ \(c, (x, y), shape) -> do
                forM_ (shapeCells w h shape) $ \(dx, dy) -> do
                    writeArray arr ((y + dy) * w + (x + dx)) c
            return arr

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
