module Problems11_20 where

import Problems1_10 (pack)

-- Problem 11
-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that
-- if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists. 
data Encoding a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified list = map toEncoding (pack list)
    where toEncoding = \x -> if (length x) == 1 then Single (head x) else Multiple (length x) (head x)

-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version. 
decodeModified :: (Eq a) => [Encoding a] -> [a]
decodeModified list = concatMap decode list
    where decode (Single a) = [a]
          decode (Multiple n a) = take n (cycle [a])

-- Problem 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
-- but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- NOTE: Not sure I understand the difference between problems 13 and 11. I think I already did this in problem 11.

-- Problem 14
-- Duplicate the elements of a list. 
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x]++[x])

-- Problem 15
-- Replicate the elements of a list a given number of times. 
repli :: [a] -> Int -> [a]
repli list n = concatMap (\x -> take n (cycle [x])) list

-- Problem 16
-- Drop every N'th element from a list. 
-- Ugly, I know.
dropEvery :: [a] -> Int -> [a]
dropEvery list n = concatMap (\(x,_) -> x) filteredList
    where filteredList = filter (\x -> (snd x) `mod` n /= 0 && (snd x) /= 0) listWithIndices
          listWithIndices = foldl (\x y -> x++[([y],1+snd(last x))]) [([],0)] list

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split list n = (first_n, rest)
    where first_n = take n list
          rest = reverse (take ((length list)-n) (reverse list))

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k,
-- the slice is the list containing the elements between the i'th and k'th element of the original list
-- (both limits included). Start counting the elements with 1. 
slice :: [a] -> Int -> Int -> [a]
slice list i k = concatMap (\(x,_) -> x) filteredList
    where filteredList = filter withinRange listWithIndices
          withinRange = (\x -> (snd x) >= i && (snd x) <= k)
          listWithIndices = foldl (\x y -> x++[([y],1+snd(last x))]) [([],0)] list

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++). 
rotate :: [a] -> Int -> [a]
rotate list n
    | n >= 0    = (drop n list) ++ (take n list)
    | otherwise = let dropAmt = (length list) + n
                  in drop dropAmt list ++ take dropAmt list

-- Problem 20
-- Remove the K'th element from a list. 
removeAt :: Int -> [a] -> (a, [a])
removeAt n list = (list !! (n-1), take (n-1) list ++ drop n list)
