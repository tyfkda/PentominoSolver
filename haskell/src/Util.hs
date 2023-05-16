module Util
    ( eachElem
    ) where

import Control.Arrow (second)

eachElem :: [a] -> [(a, [a])]
eachElem [] = []
eachElem (a: as) = (a, as) : map (second (a :)) (eachElem as)
