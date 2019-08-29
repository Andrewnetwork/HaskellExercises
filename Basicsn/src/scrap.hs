primeFactorization' :: Int -> Int -> [Int]
primeFactorization' s x | s >= x       = [s]
                        | mod x s == 0 = s : primeFactorization' (quot x s) s
                        | otherwise    = primeFactorization' x (succ s)

primeFactorization :: Int -> [(Int, Int)]
primeFactorization = counts . primeFactorization' 2

primeFactorization' :: [Int] -> Int -> [Int]
primeFactorization' (p:ps) n 
    | p >= n = [p]
    | mod n p == 0 = p : primeFactorization' (p:ps) quotient
    | otherwise = primeFactorization' ps n
    where quotient = quot n p
    
primeFactorization :: Int -> [(Int, Int)]
primeFactorization = counts . primeFactorization' primes 
