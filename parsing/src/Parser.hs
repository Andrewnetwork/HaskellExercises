{-
    Andrew Ribeiro
    andrewnetwork@gmail.com
    September 2019

    Adapted from: https://www.cs.tufts.edu/comp/150FP/archive/graham-hutton/monadic-parsing-jfp.pdf
-}
module Parser where
import Control.Arrow (first)
import Control.Applicative (liftA2,Alternative,(<|>),empty)
import Control.Monad (MonadPlus,mzero,mplus)
import Prelude hiding((++))
import Data.List (dropWhile)

newtype Parser a = Parser (String -> [(a,String)])


parse (Parser p) = p

instance Functor Parser where
    fmap fn (Parser ifn ) = Parser (fmap (first fn) . ifn)

instance Applicative Parser where
    pure a = Parser (\cs -> [(a,cs)])
    (Parser fab) <*> (Parser b) = Parser (\str -> (first . fst <$> fab str) <*> b str)

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance Alternative Parser where
    empty = Parser (\cs -> [])
    p <|> q = Parser (\cs -> parse p cs ++ parse q cs)

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

-- Definitions for parity with the paper. 
a ++ b = mplus a b
zero = mzero

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++ q) cs of
                            [] -> []
                            (x:xs) -> [x])

-- item :: Parser Char
-- item = Parser (fmap (liftA2 (,) head tail) . pure)
item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c,cs)])
                            
p :: Parser (Char,Char)
p = do c <- item 
       item
       d <- item 
       return (c,d)

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

-- A parser which scans the entire string for a character which
-- satisfies the predicate (p). 
sat2 :: (Char -> Bool) -> Parser Char
sat2 p = do c <- item
            if p c then return c else (sat2 p)

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do char c
                   zero
                   return (c:cs)
                   string cs
                   

stringA "" = pure ""
stringA (c:cs) = (:) <$> char c <*> stringA cs

-- >>> parse (string "t") "t"
-- []
--


-- >>> parse (sat2 ('c'==)) "Hello cumus!"
-- [('c',"umus!")]
--
-- >>> parse item . dropWhile (/= 'c') $ "Hello cumus!"
-- [('c',"umus!")]
--
-- >>> item . pure . dropWhile (/= 'c') $ "Hello cumus!"
-- <interactive>:833:2: error:
--     • Couldn't match expected type ‘f0 [Char] -> c’
--                   with actual type ‘Parser Char’
--     • In the first argument of ‘(.)’, namely ‘item’
--       In the expression: item . pure . dropWhile (/= 'c')
--       In the expression:
--         item . pure . dropWhile (/= 'c') $ "Hello cumus!"
--     • Relevant bindings include it :: c (bound at <interactive>:833:2)
--
-- >>> tst 
-- [('c',"umus!")]
--


-- >>> "abc" ++ "cba"
-- "abccba"
--
-- >>> :t (++)
-- (++) :: MonadPlus m => m a -> m a -> m a
--

