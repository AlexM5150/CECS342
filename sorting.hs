module Main where
    qsort :: (Ord a) => [a] -> [a]  
    qsort [] = []  
    qsort (x:xs) =   
        let smaller = qsort [a | a <- xs, a <= x]  
            bigger = qsort [a | a <- xs, a > x]  
        in  smaller ++ [x] ++ bigger 

    msort :: Ord a => [a] -> [a] -> [a]
    msort xs [] = xs
    msort [] ys = ys
    msort (x:xs) (y:ys) | x <= y    = x:msort xs (y:ys)
                        | otherwise = y:msort (x:xs) ys 
Main qsort do [10,2,5,3,1,6,7,4,2,3,4,8,9]  [1,2,2,3,3,4,4,5,6,7,8,9,10]