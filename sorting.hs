module Main where
    qsort :: (Ord a) => [a] -> [a]  
    quicksort [] = []
    quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where
        lesser = filter (< x) xs
        greater = filter (>= x) xs

    msort :: Ord a => [a] -> [a] -> [a]
    msort xs [] = xs
    msort [] ys = ys
    msort (x:xs) (y:ys) | x <= y    = x:msort xs (y:ys)
                        | otherwise = y:msort (x:xs) ys 
Main qsort do [10,2,5,3,1,6,7,4,2,3,4,8,9]  [1,2,2,3,3,4,4,5,6,7,8,9,10]