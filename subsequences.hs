{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------
-- Goal: given a list, determine the sublist with the largest sum, not including the empty list.
-----------------------------------------------------------------

import Data.List (maximumBy, foldl')
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Gen


------
-- Naive solution using brute force
------
nonemptySublists :: [a] -> [[a]]
nonemptySublists xs = go [] xs
    where go :: [[a]] -> [a] -> [[a]]
          go ps [] = ps
          go ps (x:xs) = let prefixes = (fmap (++ [x]) ps) ++ [[x]]
                         in ps ++ (go prefixes xs)
                         
maxSum :: (Num a, Ord a) => [[a]] -> (a, [a])
maxSum xss = maximum $ map (\xs -> (sum xs, xs)) xss

findMaxNaive :: (Num a, Ord a) => [a] -> Maybe (a, [a])
findMaxNaive [] = Nothing
findMaxNaive xs = (Just . maxSum . nonemptySublists) xs

findMaxSumNaive :: (Num a, Ord a) => [a] -> Maybe a
findMaxSumNaive = (fmap fst) . findMaxNaive


-----------------------------------------------------------------
-- NOTE: This one doesn't work, consider [3,-2,1,-1,3]
-- Algorithm:
--  1. Divide the list into runs of successive positive/negative numbers, merging these into Ranges
--  2. Combine sublists of the form [positive, negative, positive] when both positive numbers execeed the negative number
--  3. Select the highest sum, or the highest individual number if the entire list was non-positive.
-----------------------------------------------------------------

-- | Represents the sum of the range from start to end indices, both inclusive.
data Range a = Range !a !Int !Int deriving (Eq)
rangeVal (Range x _ _) = x

instance (Show a) => Show (Range a) where
    show (Range x s e) = "{" ++ show x ++ " [" ++ show s ++ "-" ++ show e ++ "]}"

instance (Ord a) => Ord (Range a) where
    compare (Range x1 s1 e1) (Range x2 s2 e2) = compare x1 x2 <> (flip compare) (e1 - s1) (e2 - s2)
    
annotate :: [a] -> [Range a]
annotate = zipWith (\n x -> Range x n n) [0..]

merge :: (Num a) => Range a -> Range a -> Range a
merge (Range x1 s _) (Range x2 _ e) = Range (x1 + x2) s e

condense :: (Num a, Ord a) => [Range a] -> [Range a]
condense xs = go xs
  where go [] = []
        go [x] = [x]
        go (r1:r2:rest) = let Range x _ _ = r1
                              Range y _ _ = r2 in
            if (x >= 0 && y >= 0) || (x <= 0 && y <= 0)
              then go ((merge r1 r2) : rest)
              else r1 : (go (r2 : rest))
              
chomp :: (Num a, Ord a) => [Range a] -> [Range a]
chomp [] = []
chomp [x] = [x]
chomp [x,y] = [x,y]
chomp (x:y:z:rest) = let Range a _ _ = x
                         Range b _ _ = y
                         Range c _ _ = z in
  if a > 0 && a + b >= 0 && c + b >= 0
    then chomp $ (merge (merge x y) z) : rest
    else x : (chomp $ y:z:rest)
    
findMaxFaulty :: (Num a, Ord a) => [a] -> Maybe (Range a)
findMaxFaulty [] = Nothing
findMaxFaulty xs = Just $ max naive full
  where annotated = annotate xs
        naive = maximum annotated
        full = (maximum . chomp . condense) annotated
        
findMaxFaultySum :: (Num a, Ord a) => [a] -> Maybe a
findMaxFaultySum = (fmap rangeVal) . findMaxFaulty
    
----------------------------------------------------
-- Kadane's algorithm, see https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane.27s_algorithm
----------------------------------------------------
findMax :: (Num a, Ord a) => [a] -> Maybe (Range a)
findMax [] = Nothing
findMax xs = let (y:ys) = annotate xs in Just $ snd (foldl' go (y, y) ys)
  where go (max_ending_here, max_so_far) r = let max_ending_here' = max r (merge max_ending_here r)
                                                 max_so_far' = max max_so_far max_ending_here'
                                             in (max_ending_here', max_so_far')

findMaxSum :: (Num a, Ord a) => [a] -> Maybe a
findMaxSum = (fmap rangeVal) . findMax
        
----------------------------------------------------
-- Tests
----------------------------------------------------
prop_matchesReference :: [Integer] -> Bool
prop_matchesReference xs = findMaxSumNaive xs == findMaxSum xs

prop_largest :: [Integer] -> Property
prop_largest xs = not (null xs) ==> let (Just v) = findMaxSum xs in all (<= v) xs

prop_correctRange :: [Integer] -> Property
prop_correctRange xs = not (null xs) ==> sum sublist == val
  where Just (Range val start end) = findMax xs
        sublist = take (end - start + 1) (drop start xs)
