module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
  Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)


mapExpected =
  Node (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)


mapOk =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : ((preorder left) ++ (preorder right))

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)


postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder right) ++ (postorder left) ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "preorder good"
  else putStrLn "preorder bad"


testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "inorder good"
  else putStrLn "inorder bad"


testPostorder :: IO ()
testPostorder =
  if postorder testTree == [3,1,2]
  then putStrLn "postorder good"
  else putStrLn ("postorder bad: " ++ (show $ postorder testTree))


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right

