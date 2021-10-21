import Directional

{-
This grammar implements a non-rational relation using only
what is called 'type-1 traversal' in the paper.

This grammmar takes takes as input strings that
1. start with "xxx"
2. may contain the string "sbbbaa" an arbitrary number of times (or not at all)
3. may additionally contain an arbitrary amount of a's after the "xxx" and after each instance of "sbbbaa"

It outputs:
the number of a's and s's counted in a's...
followed by xxx...
followed by the number of b's
-}

-- main runs 'converge' on an example input
main :: IO ()
main = converge grammar "xxxasbbbaaaaaasbbbaaasbbbaaaaaa"
-- output: "aaaaaaaaaaaaaaaaaaaxxxbbbbbbbbb"

grammar :: Grammar
grammar = (cons,alpha)

alpha :: Alphabet
alpha = "xyzsab"

cons :: Ranking
cons = 
    [
        -- top faith
        dep,
        max',
        topFaith,
        -- top mark
        xPos,
        zPos,
        sPos,
        yPos,
        -- outer cycle
        outerBack,
        outerFront,
        -- start inner cycle
        start1,
        start2,
        start3,
        -- inner cycle
        innerBack,
        innerFront,
        innerEnd,
        -- restart outer
        restart1,
        restart2,
        -- bottom
        faith
    ]

-- top faith
dep :: Constraint
dep = mkDep alpha
max' :: Constraint
max' = mkMax alpha
topFaith :: Constraint
topFaith = mkFaith $ changes `except` (xa ++ syz ++ zab ++ ya)
    where
        changes = [(x,y) | x <- alpha, y <- alpha]
        xa = [('x','a'),('a','x')]
        syz = [('s','y'),('s','z'),('z','s')]
        zab = [('z','a'),('a','z'),('z','b'),('b','z')]
        ya = [('y','a')]

-- top constraints
xPos :: Constraint
xPos = mkMark $ two ++ three ++ four
    where
        two = ["xz"]
        three = ["axa"]
        four = ["axxb","axx<","axxy","axxs"]
zPos :: Constraint
zPos = mkMark $ two ++ three ++ four
    where 
        two = ["zx"]
        three = ["aza","bzb","bza","azb","sza","azs","bzs"] --
        four = ["zzzz","azzb","bzza","azzs","szza","bzzs"] --
sPos :: Constraint
sPos = mkMark ["ss","sab"]
yPos :: Constraint 
yPos = mkMark ["yb","yz"]

-- middle constraints
-- outer cycle
outerBack :: Constraint
outerBack = mkMark ["ax",">x"]
outerFront :: Constraint
outerFront = mkMark ["xa","xs","xy"]

-- start inner cycle
start1 :: Constraint
start1 = mkMark ["xsb"]
start2 :: Constraint 
start2 = mkMark ["szb"]
start3 :: Constraint
start3 = mkMark ["szzb"]

-- inner cycle
innerBack :: Constraint
innerBack = mkMark ["az","bz","sz"]
innerFront :: Constraint
innerFront = mkMark ["za","zb","zs"]
innerEnd :: Constraint
innerEnd = mkFaith [('z','a'),('z','s')]

-- restart outer cycle
restart1 :: Constraint
restart1 = mkMark ["s"]
restart2 :: Constraint
restart2 = mkMark ["y"]

-- bottom constraints
faith :: Constraint
faith = mkFaith [(a,b)|a <- alpha,b <- alpha]
