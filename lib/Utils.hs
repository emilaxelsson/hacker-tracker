module Utils
    ( splitOn
    ) where

import Protolude

-- | A version of @Data.List.break@ that doesn't suffer from Boolean blindness
breakProperly :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakProperly getB = go []
  where
    go prefix [] = (reverse prefix, Nothing)
    go prefix (a : as) = case getB a of
        Nothing -> go (a : prefix) as
        Just b -> (reverse prefix, Just (b, as))

splitOn' :: (a -> Maybe b) -> (b, [a]) -> [(b, [a])]
splitOn' getB = go []
  where
    go sections (b, as) = case rest of
        Nothing -> reverse sections'
        Just x -> go sections' x
      where
        (prefix, rest) = breakProperly getB as
        sections' = (b, prefix) : sections

-- |
-- >>> splitOn (\x -> guard (x == '*') >> return x) "To buy:*Apples*Bananas"
-- ("To buy:",[('*',"Apples"),('*',"Bananas")])
splitOn :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
splitOn getB as = (prefix, sections)
  where
    (prefix, rest) = breakProperly getB as
    sections = case rest of
        Nothing -> []
        Just x -> splitOn' getB x
