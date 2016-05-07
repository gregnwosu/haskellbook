module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
 where
   go  n d count
       | n < d = (count , n)
       | otherwise = go (n-d) d (count + 1)

addIt :: IO ()
addIt = hspec $ do
  describe "Addition" $ do
    it "2 + 2 is equal to 4" $  do
      (2 + 2) `shouldBe` 4
    it "1 + 1 is greater than" $ do
       ((1::Int) + 1) > 1  `shouldBe` True
    it "15 divided by 3 is 5" $  do
      dividedBy 15 3 `shouldBe` (5,0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4,2)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

myMult :: Int -> Int -> Int
myMult n 0 = 0
myMult n x = n + (myMult n (x-1))

testMult :: IO()
testMult = hspec $ do
  describe "myMult" $ do
    it " 3 x 3 should be 9 " $ do
      myMult 3 3 `shouldBe` 9
    it " 9 x 7 should be 63" $ do
      myMult 9 7 `shouldBe` 63
    it " 10 x 0 should be 0 " $ do
      myMult 10 0 `shouldBe` 0


trivialInt :: Gen Int
trivialInt = return 1


oneThroughThree :: Gen Int
oneThroughThree = elements [1..3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [EQ .. GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)  => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a , Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple  = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

genEither :: (Arbitrary a , Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe =  arbitrary  >>=  \v -> elements  [v,Nothing]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing), (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
