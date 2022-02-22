quick :: (Ord a) => [a] -> [a]  
quick [] = []  
quick (x:xs) =   
    let smaller = quick [a | a <- xs, a <= x]  
        bigger = quick [a | a <- xs, a > x]  
    in  smaller ++ [x] ++ bigger 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys 