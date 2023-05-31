module Util
    ( eachElem
    , splitByWidth
    , unfoldTree
    , whenMaybe
    ) where

import Control.Arrow (second)
import Data.Bool (bool)
import Data.List (unfoldr)

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

splitByWidth :: Int -> [a] -> [[a]]
splitByWidth w = unfoldr $ (splitAt w <$>) . (whenMaybe =<< not . null)
-- splitByWidth w = takeWhile (not . null) . unfoldr (Just . splitAt w)

whenMaybe :: Bool -> a -> Maybe a
-- whenMaybe True  a = Just a
-- whenMaybe False _ = Nothing
whenMaybe = bool (const Nothing) Just
