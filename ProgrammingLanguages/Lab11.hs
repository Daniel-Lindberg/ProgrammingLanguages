{-# LANGUAGE ParallelListComp #-}
nbits = 17;

f :: Int -> Int -> [Char] -> Int
f modulus seed = foldl (\acc x -> ((acc*(fromEnum x) `mod` modulus)+1)) seed

hashFunctions :: [Int] -> Int -> [[Char] -> Int]
hashFunctions seeds modul = [ f modul s | s <- seeds ]

ha :: [Char] -> [Int]
ha str = [ f str | f <- (hashFunctions [34, 22, 11, 19] nbits) ]

addDataBase db idx = (last z) where z = db:[ setTrue  udb i  | udb <- z | i <- idx ]

setTrue lst i = (take i lst) ++ [True] ++ (drop (i+1) lst)

addToDB :: [Bool] -> [Char] -> [Bool]
addToDB db str = addDataBase db (ha str)

makeDataBase :: [[Char]] -> [Bool]
makeDataBase strs = (last x)
	where
		x = (replicate nbits False):[ (addDataBase a (ha str)) | a <- x | str <- strs]


query :: [Char] -> [Bool] -> Maybe Bool
query str db = if (last z) then Nothing; else Just False
	where
		z = True : [ if (db !! b) then a; else False | a <- z | b <- (ha str) ]

db = makeDataBase ["Hello", "There", "Buddies", "From", "Everywhere"]
q1 = query "hello" db
q2 = query "Close Shave" db
q3 = query "Give the man a cigar" db
q4 = query "Everywhere" db
q5 = query "buddies" db
q6 = query "Stove in" db






