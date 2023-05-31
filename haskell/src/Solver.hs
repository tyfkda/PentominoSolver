module Solver
    ( Board, Pos, Solution
    , solve
    ) where
import Data.Bits (shiftL, Bits ((.&.), (.|.)))
import Data.Maybe (mapMaybe)

import Pentomino (BitBoard, Piece, Shape)
import Util (eachElem, unfoldTree)

type Pos = (Int, Int)
type Board = (Int, Int, BitBoard)  -- w, h, bitpat
type Solution = [Char]
type Node = ([Piece], Pos, Board, [Char])

solve :: Board -> [Piece] -> [Solution]
solve board pieces = solveRecur [(pieces, (0, 0), board, [])]

solveRecur :: [Node] -> [Solution]
solveRecur = unfoldTree expandNode . Left

expandNode :: Node -> Either [Node] [Char]
expandNode ([], _, _, names)           = Right names
expandNode (pieces, pos, board, names) = Left oneStep
    where
        oneStep = concatMap f $ eachElem pieces
        f (p@(name, _), ps) = map (g ps (name:names)) $ putPiece pos board p
        g ps ns' b' = (ps, nextPos b' pos, b', ns')

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

nextPos :: Board -> Pos -> Pos
nextPos (bw, bh, bits) = head . dropWhile occupied . iterate next
    where
        next (x, y) | y + 1 < bh  = (x, y + 1)
                    | otherwise   = (x + 1, 0)
        occupied (x, y) = x < bw && y < bh && bits .&. (1 `shiftL` (y * bw + x)) /= 0
