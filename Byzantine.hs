import Data.List

-- calculates what order a given general will give to each lieutenant number
-- v is their traitor status
-- i is the lieutenant number
-- o is the order the commander recieved
-- obvious logic as described in problem description
-- returns a character, 'A' for attack or 'R' for retreat
newc v i o = if v == 'T' && even i then noto else o where noto = if o == 'A' then 'R' else 'A'

-- second row checks for frequencies of A, R, and T in previous row
-- if the row doesn't seem to be participating, leave it as ' '
-- else put A, R, or - (TIE) as appropriate
makechoice decstr = if a == 0 && r == 0 && t == 0 then ' ' else if a > r then 'A' else if a < r then 'R' else '-'
    where (a:r:t:[]) = map (\x -> length $ flip filter decstr (==x)) ['A','R','T']

-- o is the commander's order, and c is the commander number.
-- l is the "other lieutenants" from the last round- i.e. the lieutenants for this round
-- the return value is an nxn matrix (where n is # lieutenants from 1st round)
-- ' ' represents anything not relevant to this round
-- (aka either not a lieutenant or a diagonal on the matrix)
-- when the final call returns, m is the matrix that will be printed.
-- getting a given submatrix column i for a valid lieutenant is done by mapping makechoice over the
-- recursive call where that particular lieutenant is commander.
-- if a lieutenant is not participating, they get a column of ' ' which is not counted in makechoice
-- if they're the commander, the column is just the commands they issued from newc
-- base case just populates the entire row of a given lieutenant with the cmd they get from that round's commander.
byzantine 0 g o c l = [replicate (length g) $ ltval x | x <- [0..(length g-1)]]
    where ltval lt = if lt == c then ' ' else if lt `elem` l then newc (g !! c) lt o else ' '
byzantine m g o c l = transpose [if x == c then cmds else if (x `elem` l) then map makechoice $ byzantine (m-1) g (cmds!!x) x (delete x l)
        else (replicate (length g) ' ') | x <- [0..(length g-1)]]
    where cmds = [if (i `elem` l) then newc (g!!c) i o else ' ' | i <- [0..(length g-1)]]

-- constructs a list of strings from the final returned matrix
-- each line is one lieutenant for the final thing that gets printed
-- just pretty-printing and formatting here, really nothing interesting
byzprint g li = [((head $ li!!i):' ':(tail $ li!!i)) ++ (jmap i) ++ (if (g!!i) == 'T' then "*" else "")| i <- [1..(length li - 1)]]
    where jmap i = if choice == '-' then " TIE" else if choice == 'A' then " ATTACK" else " RETREAT" where choice = makechoice (li!!i)

-- read things into the correct types, get the first letter from strings to use as basically enums
-- start with G0 as cmdr, 1..n as lieutenants
byzantineparse l@[m,g,o] = byzprint g $ byzantine (read m :: Int) g (o !! 0) 0 [1..(length g - 1)]

-- get the line, split it into a list by spaces, pass to byzantineparse.
-- then putStrLn all but the last line of aaaa (aaaa is the answer)
-- then finally putStr the last line, because we don't want a newline at the end.
main = getLine >>= (\fl -> let aaaa = (byzantineparse . words) fl in (traverse putStrLn $ init aaaa) >> (putStr $ last aaaa))
