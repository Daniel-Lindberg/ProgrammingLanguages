import Data.Ratio

whosMax [] = Nothing
whosMax [a] = a
whosMax (a:x) = if (length x) == 0 then a; else (max (whosMax (x)) a)

whosMax :: Ord a => [Maybe a] -> Maybe a


foundMax [] = Nothing
foundMax [a] = Just a
foundMax (a:x) = if (length x) == 0 then Just a; else (maxf (foundMax x) a)

maxf (Just a) b = Just(max a b)

tips (a:x) = x

data KnapObj a = KnapObj a a deriving (Show)
val (KnapObj x y) = x
wgt (KnapObj x y) = y

toKnapObjs lst = [ (KnapObj a b) | (a,b) <- lst ]

data CList a =
	Empty
	| Cell a (CList a) deriving (Show)

listAdd x Empty = (Cell x Empty)
listAdd x lst = (Cell x lst)



--appendList cListObj lst = foldl cListObj (CList lst)

--listPeek [] = 0
--listPeek (a:x) = (a-> (Cell a) -> a )

sumRatios lst = foldr (+) (7%16) lst


f2 :: (Num t, Monad m) => m t -> m t -> m t -> m [t]
f2 a b c = a >>= (\x -> b >>= (\y -> c >>= (\z -> return [x*y*z])))
v3 = f2 [1%2,1%2] [1%4,3%4] [1%8,7%8]
v4 = length v3
v5 = foldl (\acc (x:xs) -> (acc+x)) 2 v3