module Week04.Monad where

-- foo::IO Int
-- foo = ....

-- () means nothing, so the main take the IO , and tranform to nothing
-- main :: IO ()
-- main = putStrLn "Hello, World"

-- getLine will wait for the user input
-- :t getLine
-- getLine :: IO String
-- :i IO

-- toUpper is given a char and returns a char
-- Prelude Data.Char> :t toUpper
-- toUpper :: Char -> Char

-- map toUpper given a char list and return a char list
-- Prelude Data.Char> :t map toUpper
-- map toUpper :: [Char] -> [Char]

-- fmap is Functor f that when take f but do it in each of the f
-- Prelude Data.Char> :t fmap (map toUpper)
-- fmap (map toUpper) :: Functor f => f [Char] -> f [Char]

-- :t fmap (map toUpper) getLine
-- fmap (map toUpper) getLine :: IO [Char]

-- fmap (map toUpper) getLine

-- Prelude Data.Char> putStrLn "Hello" >> putStrLn "World"
-- Hello
-- World
-- :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- getLine >>= putStrLn

-- Prelude Data.Char> return "Haskell" :: IO String
-- "Haskell"

-- Prelude Data.Char> :t return
-- return :: Monad m => a -> m a

-- bar :: IO ()
-- bar = getLine >>= \s ->
--       getLine >>= \t ->
--       putString (s ++ t)

------------------------------------------------------------
-- import Text.Read (readMaybe)
-- read "42" :: Int
-- 42
-- read "42+s" :: Int
-- lexical error in string/character literal at end of input
-- Prelude Text.Read> readMaybe "42" :: Maybe Int
-- Just 42
-- Prelude Text.Read> readMaybe "42d" :: Maybe Int
-- Nothing
--------------------------------------------------------------
-- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
-- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
-- bindEither :: Either String a -> (a -> Either String b) -> Either String b
-- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
--
-- return              :: a -> IO a
-- Just                :: a -> Maybe a
-- Right               :: a -> Either String a
-- (\a -> Writer a []) :: a -> Writer a

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s

threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
