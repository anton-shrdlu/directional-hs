module Directional
    ( Grammar
    , Constraint
    , Ranking
    , Candidate
    , Alphabet
    , mkMark
    , mkMax
    , mkDep
    , mkFaith
    , except
    , converge
    , converge'
    , step
    ) where

{--
This module provides the tools to define HS grammars
that use directional evaluation similar to Eisners original proposal for OT.
Markedness constraints are evaluated left to right.
Faithfulness constraints are evaluated unboundedly.
Because of the restrictions to GEN in HS, they are effectively 1-bounded. 
--}


import Data.List (findIndex, isPrefixOf, tails, minimumBy)
import Data.Maybe (isJust, isNothing, fromJust)

{--
This Section defines the basic data types of an HS grammar.
All grammars are defined for strings of Chars.
--}

type Candidate = ([Char],(Maybe Char,Maybe Char),[Char])
type Alphabet = [Char]
type Constraint = Candidate -> Maybe Int
type Ranking = [Constraint]
type Grammar = (Ranking,Alphabet)

{--
'except' takes a set (for example an alphabet) and a subset (for example labial symbols)
and returns the set difference of superset-subset.
This is useful for defining very restrictive constraints concisely. 
--}

except :: Eq a => [a] -> [a] -> [a]
except as xs = filter (`notElem` xs) as 

{--
'gen' resturns candidates for a grammar and an input string.
candidates take the form of a three tuple (a,b,c):
  a is the part of the string before the change, plus the initial symbol '>'.
  b is a tuple representing the change, where "Just 'x'" represents the character x
  and Nothing represents the empty string. 
  Nothing is needed for epenthesis and elision.
  c is the part of the string after the change, plus the final symbol '<'.
--}



gen :: Grammar -> String -> [Candidate] 
gen (ranking,alphabet) input = addEdges <$> faithful:insertions' ++ deletions' ++ replacements'
    where
        addEdges (as,c,bs) = ('>':as,c,bs++"<") 
        splits' = splits input
        faithful = (input,(Nothing,Nothing),"")
        insertions' = insertions alphabet splits'
        deletions' = deletions alphabet splits'
        replacements' = replacements alphabet splits'

{--
These are helper functions defining potential positions for changes and the
different kinds of possible changes.
--}

splits :: String -> [(String, String)]
splits as = fmap splitAt [0..length as] <*> pure as

insertions :: Alphabet -> [(String,String)] -> [Candidate]
insertions as xys = [(x,(Nothing,Just a),y) | a <- as, (x,y) <- xys]

deletions :: Alphabet -> [(String,String)] -> [Candidate]
deletions as xys = [(x,(Just $ head y,Nothing),tail y) | (x,y) <- xys, not $ null y]

replacements :: Alphabet -> [(String,String)] -> [Candidate]
replacements as xys = [(x,(Just $ head y,Just a),tail y) | (x,y) <- xys, not $ null y, a <- as, head y /= a]

{--
This section defines helper functions to define constraints
--}

-- 'mkFaith' defines faithfulness constraints, it takes a list of prohibited changes.

mkFaith' :: [(Maybe Char, Maybe Char)] -> Constraint
mkFaith' pairs (_,change,_)
    | change `elem` pairs = Just 0
    | otherwise = Nothing

-- This function helps define IDENT constraints.

mkFaith :: [(Char,Char)] -> Constraint
mkFaith pairs = mkFaith' wrapped
    where
        wrapped = fmap (\(x,y) -> (Just x,Just y)) pairs

-- This function helps define DEP constraints.

mkDep :: [Char] -> Constraint
mkDep symbols = mkFaith' pairs
    where
        pairs = [(Nothing, Just a) | a <- symbols]

-- This function helps define MAX constraints.

mkMax :: [Char] -> Constraint
mkMax symbols = mkFaith' pairs
    where
        pairs = [(Just a, Nothing) | a <- symbols]


{--
'mkMark' helps define markedness constraints.
Markedness constraints are evaluated left to right,
define positions through the alignment of outputs with the input as in Eisner 2002,
assign violation at the end of marked sequences,
and are strictly local.
--}

-- Top function to define markedness constraints

mkMark :: [String] -> Constraint
mkMark xs = \cand -> mostSevere (map adjEndIndex xs <*> pure cand) -- list comprehension would be better
    where
        mostSevere xs
            | all null xs = Nothing
            | otherwise = minimum $ filter isJust xs

-- This function adjusts indexes to align candidates with the input.

adjEndIndex :: String -> Candidate -> Maybe Int
adjEndIndex sub cand@(as,(x,y),bs)
    | isNothing unadjusted = Nothing
    | isNothing x && isNothing y = unadjusted
    | isNothing y && unadjusted > pure (length as) = (1+) <$> unadjusted
    | isNothing x && unadjusted > pure (length as) = (1-) <$> unadjusted
    | otherwise = unadjusted
    where
        unadjusted = endIndex sub (concat' cand)
        endIndex :: String -> String -> Maybe Int
        endIndex sub str = (+ length sub) <$> findIndex (isPrefixOf sub) (tails str)    

-- This section defines how to find the optimal violation profile, i.e. EVAL.

compare' :: Ord a => Maybe a -> Maybe a -> Ordering
compare' x y
    | isNothing x && isJust y = GT
    | isJust x && isNothing y = LT
    | otherwise = x `compare` y

getBests :: [Candidate] -> [Candidate] -> Constraint -> [Candidate]
getBests bs ws c
    | null bs = getBests [head ws] (tail ws) c
    | null ws = bs
    | otherwise = case c (head bs) `compare'` c (head ws) of
        GT -> getBests bs (tail ws) c
        LT -> getBests [head ws] (tail ws) c
        EQ -> getBests (head ws:bs) (tail ws) c

stepInner :: [Constraint] -> [Candidate] -> [Candidate]
stepInner cons cands
    | null cons = cands
    | length bests == 1 = bests
    | otherwise = stepInner (tail cons) bests
        where
            bests = getBests [] cands (head cons)

-- 'step' models a single round of optimization for a grammar and a candidate.

step :: Grammar -> String -> [Candidate]
step grammar@(ranking,alphabet) input = stepInner ranking candidates
    where
        candidates = gen grammar input

-- 'converge' returns all intermediate steps for a grammar and an input until convergence.

converge :: Grammar -> String -> IO ()
converge grammar xs
    | (strip . unwrap) new == xs = 
        do
        let current = xs -- redundant
        putStrLn xs
    | otherwise = 
        do
        let current = (strip . unwrap) new
        putStrLn current
        converge grammar ((strip . unwrap) new)
    where
        strip = init . tail
        unwrap = concat' . head
        new = step grammar xs

-- "converge'" returns only the final, most optimal candidate.

converge' :: Grammar -> String -> IO ()
converge' grammar xs
    | (strip . unwrap) new == xs = 
        do
        let current = xs -- redundant
        putStrLn xs
    | otherwise = 
        do
        let current = (strip . unwrap) new
        converge' grammar current
    where
        strip = init . tail
        unwrap = concat' . head
        new = step grammar xs

-- "concat'" defines how to turn a candidate back into an ordinary string.

concat' :: Candidate -> String
concat' (as,(_,y),bs)
    | isNothing y = as ++ bs
    | otherwise = as ++ pure (fromJust y) ++ bs
