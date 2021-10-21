import Directional

{--
This grammar implements the relation 'f' from the paper.
it takes a string "(cba)^n(cbaxabc)(abc)^m"
and returns "cbaxabx(abc)^(m-n)" if n<m
and "(cba)^(n-m)cbaxabc" if n>m .
--}

-- 'main' runs 'concerge' on an example input.

main = converge grammar "cbacbacbacbacbacbacbacbacbaxabcabcabcabcabcabcabcabcabc"
main :: IO ()

grammar :: Grammar
grammar = (cons,alpha)

alpha :: Alphabet
alpha = "xabc"

cons :: Ranking
cons = 
    [ top
    , faith
    , dep
    , maxX
    , right
    , left 
    , maxABC
    ]
-- top constraints
top :: Constraint
top = mkMark $ topl ++ topr ++ stop
    where
        two = []
        topl = ["axc","bxa","cxb"]
        topr = ["xac","xba","xcb"]
        stop = ["xbc<",">cbaxb"]

faith :: Constraint
faith = mkFaith changes
    where 
        changes = [(x,y) | x <- alpha, y <- alpha]

dep :: Constraint
dep = mkDep alpha 

maxX :: Constraint
maxX = mkMax "x"

-- middle constaints
right :: Constraint
right = mkMark ["ab","bc","ca"]
left :: Constraint
left = mkMark ["axb","bxc","cxa"]

-- bottom constraints
maxABC :: Constraint
maxABC = mkMax "abc"