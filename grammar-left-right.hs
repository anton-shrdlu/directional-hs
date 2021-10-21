import Directional

{--
This grammar implements the relation 'g' from the paper.
It takes a string "lc^+" and splits the string into a left half of 'l's
and a right half of 'r's. If the number of character is odd, the middle character defaults to 'l'.
--}

-- main runs 'converge' on an example input

main :: IO ()
main = converge grammar "lcccccc"

grammar :: Grammar
grammar = (cons,alpha)

alpha :: Alphabet
alpha = "sxyzclr"

cons :: Ranking
cons = 
    [ top
    , topFaith
    , topDep
    , topMax
    -- outer cycle
    , outer2
    -- start inner
    , start
    -- inner cycle
    , inner1
    , inner2
    , inner3
    -- restart outer
    , restart1
    , restart2
    , restart3
    -- bottom 
    , bottomFaith
    ]
-- top constraints
top :: Constraint
top = mkMark $ two ++ three
    where
        two = ["zc","z<","zz","zs","ys","yz","cs","ss","rs","ry","rz","rc","rx"] --rl
        three = ["zyy","yyy","cyr","lyr"] --lyr

topFaith :: Constraint
topFaith = mkFaith $ changes `except` (ycycle ++ xcycle)
    where 
        changes = [(x,y) | x <- alpha, y <- alpha]
        ycycle = [('c','y'),('y','z'),('z','c'),('z','s'),('s','x'),('y','r')]
        xcycle = [('x','l')]

topDep :: Constraint
topDep = mkDep alpha

topMax :: Constraint
topMax = mkMax alpha

-- outer cycle
outer2 :: Constraint
outer2 = mkMark ["lx","ly","lz","ls","lc"]

-- start inner
start :: Constraint
start = mkMark ["lc"]

-- inner cycle
inner1 :: Constraint
inner1 = mkMark ["yc"]
inner2 :: Constraint
inner2 = mkMark ["yy"]
inner3 :: Constraint
inner3 = mkMark ["zy"]

-- restart outer
restart1 :: Constraint
restart1 = mkMark ["y"]
restart2 :: Constraint
restart2 = mkMark ["z"]
restart3 :: Constraint
restart3 = mkMark ["s"]

-- bottom 
bottomFaith :: Constraint
bottomFaith = mkFaith changes
    where 
        changes = [(x,y) | x <- alpha, y <- alpha]