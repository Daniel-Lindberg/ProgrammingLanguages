-- The answer to number 1 is a list of strings

--Number two

knapsack :: [ ( Int, Int ) ] -> [ ( Int, Int ) ] -> Int -> [ ( Int, Int ) ]
knapsack xs [] _   = xs
knapsack xs ys max =
    foldr (maxOf) [ ] ( xs : [ knapsack ( y : xs ) ( ys #- y ) max
                             | y <- ys, weightOf( y : xs ) <= max ] )

(#-) :: [ ( Int, Int ) ] -> ( Int, Int ) -> [ ( Int, Int ) ]
[ ]        #- _ = [ ]
( x : xs ) #- y = if x == y then xs else x : ( xs #- y )

maxOf :: [ ( Int, Int ) ] -> [ ( Int, Int ) ] -> [ ( Int, Int ) ]
maxOf a b = if valueOf a > valueOf b then a else b

valueOf :: [ ( Int, Int ) ] -> Int
valueOf [ ]        = 0
valueOf ( x : xs ) = fst x + valueOf xs

weightOf :: [ ( Int, Int ) ] -> Int
weightOf [ ]        = 0
weightOf ( x : xs ) = snd x + weightOf xs

--Example on how to call:
--knapsack [] [(2,1), (3,2), (4,3), (6,4)] 5
--Result: [(2,1),(6,4)]