module Solver
    ( Board, Pos
    , solve
    ) where
import Data.Bits (shiftL, Bits ((.&.), (.|.)))
import Data.Maybe (mapMaybe)

import Pentomino (BitBoard, Piece, Shape)
import Util (eachElem)

type Pos = (Int, Int)
type Board = (Int, Int, BitBoard)  -- w, h, bitpat

solve :: Board -> [Piece] -> [(Board, [Piece])]
solve board pieces = solveRecur (0, 0) board pieces

solveRecur :: Pos -> Board -> [Piece] -> [(Board, [Piece])]
solveRecur pos board pieces = concatMap f $ eachElem pieces
    where
        f (p, ps) = map (g ps) $ putPiece pos board p
        g ps b' = (b', ps)

putPiece :: Pos -> Board -> Piece -> [Board]
putPiece pos board (_, shapes) = mapMaybe f shapes
    where
        f shape = putShape pos board shape

putShape :: Pos -> Board -> Shape -> Maybe Board
putShape (x, y) (bw, bh, boardbits) (shapebits, sw, sh, ofsy)
    | inRange && noConflict  = Just (bw, bh, boardbits')
    | otherwise              = Nothing
    where
        inRange = x + sw <= bw && y - ofsy >= 0 && y - ofsy + sh <= bh
        noConflict = (boardbits .&. (shapebits `shiftL` ((y - ofsy) * bw + x))) == 0
        boardbits' = boardbits .|. (shapebits `shiftL` ((y - ofsy) * bw + x))
