module Trees where


data BinaryTree a = None | Tree a Int (BinaryTree a) (BinaryTree a) deriving (Show)

--listToTree :: [Int] -> BinaryTree
listToTree :: (Ord a) => [a] -> BinaryTree a
listToTree ls = listToTree' ls None
listToTree' (x:[]) initTree = insertIntoTree x initTree
listToTree' (x:xs) initTree = listToTree' xs (insertIntoTree x initTree)
--listToTree [1,4,5,6]
--listToTree [1,-5,5,4,6]
--listToTree [1,-5,5,5,4,6]

insertIntoTree :: (Eq a, Ord a) => a -> BinaryTree a -> BinaryTree a
insertIntoTree elm None = Tree elm 1 None None
insertIntoTree elm (Tree parent count l r)
  | elm < parent = Tree parent count (insertIntoTree elm l) r
  | elm == parent = Tree parent (count+1) l r
  | otherwise = Tree parent count l (insertIntoTree elm r)
-- insertIntoTree 4 (insertIntoTree 3 None)

treeToList :: BinaryTree a -> [a]
treeToList tree = concat.treeToList' $ tree
treeToList' None = []
treeToList' (Tree parent count l r) = leftBranch:take count (repeat parent):rightBranch
                                      where leftBranch = concat.treeToList' $ l
                                            rightBranch = treeToList' $ r
-- treeToList.listToTree $ [1,1,4,5,6]
-- treeToList.listToTree $ [6,5,4,3,2]
-- treeToList.listToTree $ [1,1,-1,-2,4,9,5,6]

btreeSort :: Ord a => Bool -> [a] -> [a]
btreeSort accending ls
  | accending = acLs
  | otherwise = reverse acLs
  where acLs = treeToList.listToTree $ ls
