-- Symmetry.hs
-- December 2018
-- Andrew Ribeiro

module Symmetry where
import Data.List (transpose,elemIndex,nub,intercalate)
import Data.List.Split

data Symmetry a = Symmetry [[a]] [Bool]
data SymmetryType = DS | CS | HS | VS | I deriving Show
data SymTree a = Empty | SymTree SymmetryType (Matrix a) [SymTree a]
type Matrix a = [[a]]
type SymmetryPath = [SymmetryType]

getSymFlags :: Symmetry a -> [Bool]
getSymFlags (Symmetry _ flags) = flags

makeSym :: Eq a => [[a]] -> Symmetry a
makeSym mat = Symmetry mat (symFlags mat)

-- ####### INSTANCES AND IO ############
instance (Show a) => (Show (Symmetry a)) where
    show (Symmetry ls conds)
        | any (True==) conds = boardStr ++ (strSymConds conds) ++ "\n"
        | otherwise = boardStr ++ "No symmetry.\n"
        where boardStr = "\n"++concatMap (\x->x++"\n") (map show ls)

instance (Show a) => (Show (SymTree a)) where
    show Empty = show "Empty"
    show (SymTree symType parent children) = (show symType)++(show parent)++(show children)

formatMatrix :: (Show a) => [[a]] -> String
formatMatrix []   = "┌─┐\n└─┘\n"
formatMatrix [[]] = "┌─┐\n└─┘\n"
formatMatrix xxs  = (++ bot) $ concat $ zipWith (\a b -> unlines [a, b]) (top : replicate rowC mid) rows
    where
    rowC   = pred . length $ xxs
    colC   = pred . length . head $ xxs
    top    = "┌" ++ repC "─┬" ++ "─┐"
    mid    = "├" ++ repC "─┼" ++ "─┤"
    bot    = "└" ++ repC "─┴" ++ "─┘"
    repC  = concat . replicate colC
    rows   = (++ "|") . ('|' :) . intercalate "|" . ((\x -> show x) <$>) <$> xxs

prettyPrintMatrix :: (Show a) => [[a]] -> IO ()
prettyPrintMatrix  = putStrLn . formatMatrix

strSymConds :: [Bool] -> [Char]
strSymConds conds = concatMap (\x -> snd x) (filter (\x -> fst x) (zip conds ["D","C","V","H"]))
-- #########################################

-- ####### Symmetries ############
vert :: [[a]] -> [[a]]
vert board = map reverse board

horz :: [a] -> [a]
horz board = reverse board

-- Transposing a board's rows reflects across the diagonal.
-- Transposition is making the columns of the board, the rows of the board.
diag :: [[a]] -> [[a]]
diag board = transpose board

cdiag :: [[a]] -> [[a]]
cdiag board = diag.vert.horz $ board

makeGrid :: Int -> Int -> [[Int]]
makeGrid rows cols = chunksOf cols [1..rows*cols]

-- Given a matrix M, determine if it is symmetric with respect to cdiag,diag,horz,vert.
symFlags :: Eq a => [[a]] -> [Bool]
symFlags m = map (\x -> (x m)==m) [diag,cdiag,vert,horz]
-- #######################################

-- ✓ Problem: Given two matricies a,b determine what symmetric operations can be
--             perfomed on a in order to produce b.
symPath :: Eq a => Matrix a -> Matrix a -> [SymmetryPath]
symPath a b = map (\(path,mat)->path) (filter (\(path,mat)->mat==b) symPaths)
              where symPaths = getSymPaths [] (symTree I a [])


symPathToFunc :: SymmetryPath -> (Matrix a -> Matrix a)
symPathToFunc [] = id
symPathToFunc (x:xs) = case x of
                        DS -> diag.(symPathToFunc xs)
                        CS -> cdiag.(symPathToFunc xs)
                        HS -> horz.(symPathToFunc xs)
                        VS -> vert.(symPathToFunc xs)
                        I  -> id.(symPathToFunc xs)


verifyPaths :: Eq a => Matrix a -> Matrix a -> [SymmetryPath] -> Bool
verifyPaths a b paths = any (\path -> (path a) == b) (map symPathToFunc paths)

makeSymTree :: Eq a => [[a]] -> SymTree a
makeSymTree mat = symTree I mat []

symTree :: Eq a => SymmetryType -> [[a]] -> [[[a]]] -> SymTree a
symTree symType mat hist
    | elem mat hist = Empty
    | otherwise = SymTree symType mat [diagChild,cdiagChild,horzChild,vertChild]
    where diagChild = symTree DS (diag mat) (mat:hist)
          cdiagChild = symTree CS (cdiag mat) (mat:hist)
          horzChild = symTree HS (horz mat) (mat:hist)
          vertChild = symTree VS (vert mat) (mat:hist)

lenSymTree :: Num p => SymTree a -> p
lenSymTree Empty = 0
lenSymTree (SymTree _ _ children) = foldl (+) 1 (map lenSymTree children)

getParents :: SymTree a -> [[[a]]]
getParents Empty = []
getParents (SymTree _ board children) = board:(concatMap getParents children)

getSymPaths :: SymmetryPath -> SymTree a -> [(SymmetryPath,Matrix a)]
getSymPaths symPath Empty  = []
getSymPaths symPath (SymTree sym board children) = (sym:symPath,board):(concatMap (getSymPaths (sym:symPath)) children)
