-- BinaryCounterTree.hs
-- Andrew Ribeiro
-- December 2018

module BinaryCounterTree where

data BinaryCounterTree a = None | Tree a Int (BinaryCounterTree a) (BinaryCounterTree a) deriving (Show)

listToTree :: (Ord a) => [a] -> BinaryCounterTree a
listToTree ls = listToTree' ls None
listToTree' (x:[]) initTree = insertIntoTree x initTree
listToTree' (x:xs) initTree = listToTree' xs (insertIntoTree x initTree)
--listToTree [1,4,5,6]
--listToTree [1,-5,5,4,6]
--listToTree [1,-5,5,5,4,6]

insertIntoTree :: (Eq a, Ord a) => a -> BinaryCounterTree a -> BinaryCounterTree a
insertIntoTree elm None = Tree elm 1 None None
insertIntoTree elm (Tree parent count l r)
  | elm < parent = Tree parent count (insertIntoTree elm l) r
  | elm == parent = Tree parent (count+1) l r
  | otherwise = Tree parent count l (insertIntoTree elm r)
-- insertIntoTree 4 (insertIntoTree 3 None)

treeToList :: BinaryCounterTree a -> [a]
treeToList tree = concat.treeToList' $ tree
treeToList' None = []
treeToList' (Tree parent count l r) = leftBranch:take count (repeat parent):rightBranch
                                      where leftBranch = concat.treeToList' $ l
                                            rightBranch = treeToList' $ r

treeNub  ls =  concat.treeNub' $ tree
               where tree = listToTree ls
treeNub' None = []
treeNub' (Tree parent count l r) = leftBranch:[parent]:rightBranch
                                   where leftBranch = concat.treeNub' $ l
                                         rightBranch = treeNub' $ r
-- treeToList.listToTree $ [1,1,4,5,6]
-- treeToList.listToTree $ [6,5,4,3,2]
-- treeToList.listToTree $ [1,1,-1,-2,4,9,5,6]

btreeSort :: Ord a => Bool -> [a] -> [a]
btreeSort accending ls
  | accending = acLs
  | otherwise = reverse acLs
  where acLs = treeToList.listToTree $ ls

instance Functor BinaryCounterTree where
  fmap f None = None
  fmap f (Tree parent counter left right) = Tree (f parent) counter (fmap f left) (fmap f right)

instance Foldable BinaryCounterTree where
  foldMap f None = mempty
  foldMap f (Tree parent counter left right)  = foldMap f left `mappend` f parent `mappend` foldMap f right

countElem :: (Ord a) => a -> BinaryCounterTree a -> Int
countElem elm None = 0
countElem elm (Tree parent counter left right)
  | elm < parent = countElem elm left
  | elm > parent = countElem elm right
  | otherwise = counter

allCounts :: BinaryCounterTree a -> [(a,Int)]
allCounts tree = concat.allCounts' $ tree
allCounts' None = []
allCounts' (Tree parent count l r) = leftBranch:[(parent,count)]:rightBranch
                                     where leftBranch = concat.allCounts' $ l
                                           rightBranch = allCounts' $ r


-- concatMap collatzSeq [1..1000000000]

-- filter (\(n,c)->c>=2) (allCounts.listToTree $ collatzSeq 1000)
-- filter (\(n,c)->c>=2) (allCounts.listToTree $ collatzSeq 40)

-- allCounts (listToTree [1,1,4,5,2,1,6])
-- allCounts.listToTree $ collatzSeq 8000
-- filter (\(n,c)->c>=2) (allCounts.listToTree $ collatzSeq 8000)
-- countElem 100 (listToTree $ collatzSeq 8000)
-- countElem 1 (listToTree $ [1,1,4,5,2,1,6])
-- fmap (succ.succ) (listToTree $ [1,1,4,5,6])
-- fmap (2==) (listToTree $ [1,1,4,5,2,6])
-- any (2==) (listToTree $ [1,1,4,5,2,6])
-- sum (listToTree $ [1,1,4,5,2,6])

-- foldr (+) 3 (listToTree $ [1,1,4,5,2,6])
-- foldr (+) 0 (listToTree $ [1,1])
-- foldMap (3==) (listToTree $ [1,1,4,5,3,6])
-- elem 3 (listToTree $ [1,1,4,2,2,6])
