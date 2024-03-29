module Pentomino
    ( BitBoard, Piece, Shape
    , createPentominos
    , shapeCells
    ) where
import Data.Bits (complement, countTrailingZeros, shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int64)
import Data.List (foldl', nub, unfoldr)

type BitBoard = Int64
type Shape = (BitBoard, Int, Int, Int)  -- bitpat, w, h, ofsx

toShape :: [String] -> Shape
toShape ss = (bitpat, w, h, ofsx)
    where
        bitpat = foldl' f 0 $ concat ss
        f acc c = (acc `shiftL` 1) + tobin c
        tobin ' ' = 0
        tobin _   = 1
        h = length ss
        w = length $ head ss
        ofsx = offsetX bitpat

shapeCells :: Shape -> [Int]
shapeCells (bitpat, _sw, _sh, _) = unfoldr f bitpat
    where
        f b | b == 0    = Nothing
            | otherwise = Just (countTrailingZeros b, b .&. (b - 1))

toBoardSize :: Int -> Int -> Shape -> Shape
toBoardSize bw _ (bitpat, sw, sh, ofsx) = (bitpat', sw, sh, ofsx)
    where
        bitpat'
            | sw == bw  = bitpat
            | otherwise = foldl' f bitpat $ reverse [1..sh - 1]
        f b y = let mask = (1 `shiftL` (y * sw)) - 1
                    negMask = complement mask
                 in (b .&. mask) .|. ((b .&. negMask) `shiftL` (bw - sw))

type Piece = (Char, [Shape])

toPiece :: Int -> Int -> (Char, [String]) -> Piece
toPiece bw bh (name, ss) = (name, shapes)
    where
        shapes = map (toBoardSize bw bh) $ nub [s | s@(_, sw, sh, _) <- rotate4 base_shape ++ rotate4 flipped_shape, sw <= bw && sh <= bh]
        rotate4 = take 4 . iterate rot90
        base_shape = toShape ss
        flipped_shape = toShape $ reverse ss

rot90 :: Shape -> Shape
rot90 (bitpat, w, h, _) = (bitpat', h, w, ofsx)
    where
        bitpat' = foldl' f 0 [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
        f acc (x, y) = acc .|. (bit2d (w - 1 - x) y `shiftL` (x * h + y))
        bit2d x y = (bitpat `shiftR` (y * w + x)) .&. 1
        ofsx = offsetX bitpat'

offsetX :: BitBoard -> Int
offsetX = countTrailingZeros

createPentominos :: Int -> Int -> [Piece]
createPentominos bw bh = map (toPiece bw bh) [
    ('F', [
        " @@",
        "@@ ",
        " @ "]),
    ('I', [
        "@",
        "@",
        "@",
        "@",
        "@"]),
    ('L', [
        "@ ",
        "@ ",
        "@ ",
        "@@"]),
    ('N', [
        " @",
        " @",
        "@@",
        "@ "]),
    ('P', [
        "@@",
        "@@",
        "@ "]),
    ('T', [
        "@@@",
        " @ ",
        " @ "]),
    ('U', [
        "@ @",
        "@@@"]),
    ('V', [
        "@  ",
        "@  ",
        "@@@"]),
    ('W', [
        "@  ",
        "@@ ",
        " @@"]),
    ('X', [
        " @ ",
        "@@@",
        " @ "]),
    ('Y', [
        " @",
        "@@",
        " @",
        " @"]),
    ('Z', [
        "@@ ",
        " @ ",
        " @@"])
    ]
