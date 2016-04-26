module Chapter11BinTree where

import Data.Char

data BinaryTree a  = Leaf | Node (BinaryTree a) a (BinaryTree a)
                     deriving (Eq, Show , Ord)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert'  b n@(Node left a right)
 | a == b = n
 | b > a = Node left a (insert' b right)
 | b < a = Node (insert' b left) a right


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
     then print "yupOKay"
     else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l x r) = x : preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l x r) =  inorder l ++ x: inorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l x r) =  postorder l ++ postorder r ++ [x]


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder:: IO()
testPreorder = if preorder testTree == [2,1,3] then
                 putStrLn "Preorder fine!"
                 else
                 putStrLn "bad news preorder"


testInorder:: IO()
testInorder = if inorder testTree == [1,2,3] then
                 putStrLn "inorder fine!"
                 else
                 putStrLn "bad news inorder"

testPostorder:: IO()
testPostorder = if postorder testTree == [1,3,2] then
                 putStrLn "postorder fine!"
                 else
                 putStrLn "bad news postorder"
test = do
  testPreorder
  testInorder
  testPostorder

isSubSequenceOf :: (Eq a ) => [a] -> [a] -> Bool
isSubSequenceOf _ [] = False
isSubSequenceOf [] _ = True
isSubSequenceOf (x:xs) l = x `elem` l && isSubSequenceOf xs l

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (cap) .  foldr go []
  where
    cap ([],_) = ("","")
    cap (w@(l: ls), _)  = (w,toUpper l: ls)
    go ' ' tups = ("",""):tups
    go l ((s1,_):ts) = let wd = l: s1
                       in (wd,wd):ts
    go x [] =[([x],[x])]


capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs
