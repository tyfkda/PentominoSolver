module Util
    ( eachElem
    , unfoldTree
    ) where

import Control.Arrow (second)

eachElem :: [a] -> [(a, [a])]
eachElem [] = []
eachElem (a: as) = (a, as) : map (second (a :)) (eachElem as)

-- Depth First Search: Collect Leafs.
--   Left=Nodes, Right=Leaf
unfoldTree :: (a -> Either [a] b) -> Either [a] b -> [b]
unfoldTree f = either onLeft onRight
    where
        onLeft = concatMap (unfoldTree f) . map f
        onRight = pure
