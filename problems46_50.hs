module Problems46_50 where

-- Problem 46
-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
-- Example:
-- (table A B (and A (or A B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not (a && b)

nor' :: Bool -> Bool -> Bool
nor' a b = not (a || b)

xor' :: Bool -> Bool -> Bool
xor' a b = (a && not b) || (not a && b)

impl' :: Bool -> Bool -> Bool
impl' a b = not a || a && b

equ' :: Bool -> Bool -> Bool
equ' a b = a && b || (not a && not b)

truth_rows :: (Bool -> Bool -> Bool) -> [String]
truth_rows fn = do a <- [True, False]
                   b <- [True, False]
                   return (show a ++ " | " ++ show b ++ " | " ++ show (fn a b))

table :: (Bool -> Bool -> Bool) ->  IO ()
table fn = mapM_ print $ truth_rows fn
