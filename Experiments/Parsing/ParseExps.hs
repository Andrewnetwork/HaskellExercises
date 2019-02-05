-- Source: https://en.wikibooks.org/wiki/Haskell/ParseExps
import Text.ParserCombinators.ReadP

data Tree = Branch Tree Tree | Leaf deriving Show
-- Branch Leaf Leaf
-- Branch (Branch Leaf Leaf) Leaf

leaf :: ReadP Tree
leaf = do char 'o'
          return Leaf
-- leaf '0'
-- char 'f'
-- char :: Char -> ReadP Char

branch = do a <- leaf
            char '&'
            b <- tree
            return (Branch a b)

tree = leaf +++ branch

brackets p = do char '('
                r <- p
                char ')'
                return r

-- readP_to_S tree "o&o" -> [(Leaf,"&o")]
-- readP_to_S tree "o&o" -> [(Leaf,"&o"),(Branch Leaf Leaf,"")]

 -- readP_to_S tree "o&o"
 -- readP_to_S tree "o&f"

 -- Task: Write a parser that accepts strings of alternating characters.
 -- Accepted examples: "ababab","ab",...
 -- Rejected examples: "aab", "babaa", ...

-- Task: Accept "ab" strings, reject any other string.
abParser :: ReadP Bool
abParser = do char 'a'
              char 'b'
              return True

-- GOOD WAY
isABStr str = (length result) /= 0
              where result = readP_to_S (many abParser >> eof) str

-- BAD WAY
isABStr' :: String -> Bool
isABStr' str
  | str == "" = True
  | (length possibleParses) >= 1 = isABStr $ snd $ head possibleParses
  | otherwise = False
  where possibleParses = readP_to_S abParser str


-- isABStr "aaa"

abMany :: ReadP [Bool]
abMany = many abParser
-- readP_to_S abMany "abab"
-- readP_to_S abMany "abc"

-- isABStr :: String -> Bool
-- isABStr str
--   | (length $ readP_to_S abParser str) >= 1 = True
--   | otherwise = False

-- isABStr "ab"
-- isABStr "ac"
-- isABStr "abc"
-- isABStr "aba"
-- readP_to_S abParser "abc"
-- readP_to_S abParser "ab"
-- readP_to_S abParser "abcdefg"
-- readP_to_S (abParser +++ abParser) "abc"
