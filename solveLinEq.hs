import Data.List (sortOn, transpose, uncons)

solveLinEq :: (Fractional a, Eq a) => [[a]] -> [[a]]
solveLinEq = mult . rev . reduce . rev . reduce . rowEchelon

rowEchelon :: (Fractional a, Eq a) => [[a]] -> [[a]]
rowEchelon = sortOn (length . takeWhile (== 0))

-- here we assume that the matrix is in row echelon form
reduce :: (Fractional a, Eq a) => [[a]] -> [[a]]
reduce [] = []
reduce [x] = [x]
reduce (x : xs) = x : map (0:) (reduce sub)
  where
    multi = map ((/ head x) . head) xs
    pre = zip (map (zip x) xs) multi
    sub = map (\(l, m) -> map (\(a, b) -> b - a*m) . tail $ l) pre

rev :: (Fractional a, Eq a) => [[a]] -> [[a]]
rev = reverse . reverseFront

reverseFront :: (Fractional a, Eq a) => [[a]] -> [[a]]
reverseFront = map (maybe [] (\(a, b) -> b ++ [a]) . uncons . reverse)
    -- should throw error if list is empty

mult :: (Fractional a, Eq a) => [[a]] -> [[a]]
mult m = zipWith (\a b -> map (/ (a !! b)) a) m [0..(length m - 1)]