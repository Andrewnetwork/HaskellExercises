-- Trees.hs
-- Andrew Ribeiro
-- December 2018

module Trees where
data BinaryCounterTree a = None | Tree a Int (BinaryCounterTree a) (BinaryCounterTree a) deriving (Show)

listToTree :: (Ord a) => [a] -> BinaryCounterTree a
listToTree ls = listToTree' ls None
listToTree' (x:[]) initTree = insertIntoTree x initTree
listToTree' (x:xs) initTree = listToTree' xs (insertIntoTree x initTree)

insertIntoTree :: (Eq a, Ord a) => a -> BinaryCounterTree a -> BinaryCounterTree a
insertIntoTree elm None = Tree elm 1 None None
insertIntoTree elm (Tree parent count l r)
  | elm < parent = Tree parent count (insertIntoTree elm l) r
  | elm == parent = Tree parent (count+1) l r
  | otherwise = Tree parent count l (insertIntoTree elm r)

treeToList :: BinaryCounterTree a -> [a]
treeToList tree = concat.treeToList' $ tree
treeToList' None = []
treeToList' (Tree parent count l r) = leftBranch:take count (repeat parent):rightBranch
                                      where leftBranch = concat.treeToList' $ l
                                            rightBranch = treeToList' $ r
treeNub :: Ord a => [a] -> [a]
treeNub  ls =  concat.treeNub' $ tree
              where tree = listToTree ls

treeNub' :: BinaryCounterTree a -> [[a]]
treeNub' None = []
treeNub' (Tree parent count l r) = leftBranch:[parent]:rightBranch
                                   where leftBranch = concat.treeNub' $ l
                                         rightBranch = treeNub' $ r

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
