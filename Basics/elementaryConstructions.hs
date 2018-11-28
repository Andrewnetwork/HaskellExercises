-- elementaryConstructions.hs
-- Andrew Ribeiro
-- November 2018

-- #### Lists ####


-- Reversing a list:
reverseLs :: [a] -> [a]
reverseLs [x] = [x]
reverseLs (x:xs) = (reverseLs xs)++[x]
-- Examples:
-- reverseLs [1,2,3]
-- reverseLs [ [1,2],[3,4],[5,6] ]
-- map reverseLs [ [1,2],[3,4],[5,6] ]
-- reverseLs (map reverseLs [ [1,2],[3,4],[5,6] ]) == map reverseLs (reverseLs [ [1,2],[3,4],[5,6] ])

