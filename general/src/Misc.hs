module Misc where

import Data.List (foldl')

sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

doPutStrLn :: IO ()
doPutStrLn = do
  putStrLn "One"
  putStrLn "Two"
  putStrLn "Three"
  putStrLn "Four"

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " "  ++ show acctNum

-- Fibonacci one liner!
fibonacci :: [Integer]
fibonacci = 1 : scanl (+) 1 fibonacci

-- Algebraic Data Types

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

-- Binary Tree

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert b Leaf = Node Leaf b Leaf
insert b (Node left a right)
  | b == a =  Node left a right
  | b < a  =  Node (insert b left) a right
  | b > a  =  Node left a (insert b right)
insert _ Node{} = Leaf

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

testTree :: BinaryTree Integer
testTree =
  Node (Node (Node Leaf 5 Leaf) 1 (Node Leaf 3 Leaf)) 2 (Node (Node Leaf 1 Leaf) 9 (Node Leaf 8 Leaf))

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ (x : inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ start Leaf = start
foldTree f start (Node left x right) = f x (foldTree f start left) (foldTree f start right)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f = foldTree (\ x l r -> Node l (f x) r) Leaf

-- Maybe/Either

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [_] = Nothing
safeTail (_:xs) = Just xs

-- Strings

replaceWord :: String -> String -> String -> String
replaceWord target replacement word =
  if word == target
    then replacement
    else word

replaceThe :: String -> String
replaceThe s = unwords $ map (replaceWord "the" "a") $ words s

countThe :: String -> Integer
countThe = foldl' (\ occs word -> if word == "the" then occs + 1 else occs) 0 . words

startsWithVowel :: String -> Bool
startsWithVowel []    = False
startsWithVowel (s:_) = s `elem` "aeiou"

countWordBeforeVowel :: String -> [String] -> Integer
countWordBeforeVowel _ [] = 0
countWordBeforeVowel _ [_] = 0
countWordBeforeVowel t (x:y:xs) =
  if x == t && startsWithVowel y
    then countWordBeforeVowel t xs + 1
    else countWordBeforeVowel t xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countWordBeforeVowel "the" . words

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

countVowels :: String -> Integer
countVowels []     = 0
countVowels (x:xs) =
  if isVowel x
    then countVowels xs + 1
    else countVowels xs

countConsonants :: String -> Integer
countConsonants []     = 0
countConsonants (x:xs) =
  if (not . isVowel) x
    then countConsonants xs + 1
    else countConsonants xs

-- Validation

newtype Word' =
  Word' String
  deriving (Eq, Show)

countVCs :: String -> (Int, Int)
countVCs = foldl' f (0, 0)
  where f (v, c) x = if isVowel x then (v + 1, c ) else (v , c + 1 )

makeWord :: String -> Maybe Word'
makeWord str =
  if uncurry (<) counts
    then Nothing
    else Just (Word' str)
  where counts = countVCs str
