module WordNumberTest where
import Data.Char
import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)
import Data.List (sort)


main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want " $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,0,0] for 100" $ do
       digits 100 `shouldBe` [1,0,0]
  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

half ::  Double  -> Double
half = (/2)

halfIdentity :: Double -> Bool
halfIdentity = (==) <$> (*2) . half <*> id


listOrdered :: (Ord a ) => [a] -> Bool
listOrdered [_] = True
listOrdered [] = True
listOrdered (a:b:xs) = a <= b && listOrdered xs

sortedAreAlwaysOrderedProperty :: (Ord a) => [a] -> Bool
sortedAreAlwaysOrderedProperty  = listOrdered . sort

ordListGen :: (Arbitrary a) => Gen [a]
ordListGen = listOf arbitrary

checkLists :: Property
checkLists = forAll (ordListGen :: Gen [Int])
   sortedAreAlwaysOrderedProperty

plusAssociative :: (Num a, Eq a) => a -> a-> a -> Bool
plusAssociative x y z= x + (y + z) == ( (x + y) + z)

plusCommutative :: (Num a, Eq a) =>  a-> a -> Bool
plusCommutative x y = x + y == ( y + x)

multAssociative :: (Num a, Eq a) => a -> a-> a -> Bool
multAssociative x y z= x * (y * z) == ( (x * y) * z)
multCommutative :: (Num a, Eq a) =>  a-> a -> Bool
multCommutative x y = x * y == ( y * x)

powAssociative :: (Integral a, Eq a) => a -> a-> a -> Bool
powAssociative x y z= x ^ (y ^ z) == ( (x ^ y) ^ z)
powCommutative :: (Integral a, Eq a) =>  a-> a -> Bool
powCommutative x y = x ^ y == ( y ^ x)


quotProp :: (Integral a, Eq a) => a -> a -> Bool
quotProp x y = quot x y *y + rem x y == x

divProp :: (Integral a , Eq a) => a -> a -> Bool
divProp x y = div x y * y + mod x y  == x

applyProp :: (Eq b) => (a -> b) -> a -> Bool
applyProp f a = f a == (f $ a)

composeProp :: (Eq c) => (b -> c ) -> (a -> b) -> a  -> Bool
composeProp f g x = f (g x) == (f . g $ x)




runChecks:: IO()
runChecks = do
  quickCheck halfIdentity
  quickCheck checkLists
  quickCheck (sortedAreAlwaysOrderedProperty :: [String] -> Bool)
  quickCheck $ forAll (arbitrary ::  Gen (Int,Int)) ( uncurry plusCommutative )
  quickCheck $ forAll (arbitrary ::  Gen (Int,Int,Int)) (\(a,b,c) -> plusAssociative a b c)
  quickCheck $ forAll (arbitrary ::  Gen (Int,Int)) ( uncurry multCommutative )
  quickCheck $ forAll (arbitrary ::  Gen (Int,Int,Int)) (\(a,b,c) -> multAssociative a b c)
  quickCheck $ forAll (arbitrary ::  Gen (Int,Int)) (uncurry quotProp)
  quickCheck $ forAll (arbitrary ::  Gen (Int,Int)) (uncurry divProp)
  quickCheck $ forAll (arbitrary :: Gen [String]) ((==) <$> reverse . reverse <*> id)
isPowAssoc :: IO()
isPowAssoc = do
  quickCheck $ forAll (arbitrary :: Gen(Int,Int)) (uncurry powAssociative)
isPowCommut :: IO()
isPowCommut = do
  quickCheck $ forAll (arbitrary :: Gen(Int,Int,Int)) (\(a,b,c) -> powAssociative a b c)

doesFoldConsEqAppend :: IO()
doesFoldConsEqAppend = do
  quickCheck $ forAll (arbitrary :: Gen (String,String)) (\(l1,l2) -> foldr (:) l1 l2 == l1 ++ l2)

doesFoldAppZeroEqConcat :: IO()
doesFoldAppZeroEqConcat= do
  quickCheck $ forAll (arbitrary :: Gen [String]) (\l1 -> foldr (++) [] l1 == concat l1 )

readShowProp :: (Eq a, Read a, Show a) => a -> Bool
readShowProp = (==) <$> read . show  <*> id

lengthProp :: Int -> [Int] -> Bool
lengthProp n xs =  length (take n xs ) == n

doesLengthProp = quickCheck lengthProp

doesReadShow = quickCheck (readShowProp :: Int -> Bool)

square x = x * x
sqIdProp :: (Eq a, Floating a) => a -> Bool
sqIdProp = (==) <$>square .sqrt <*> id

doesSqIdProp = quickCheck (sqIdProp :: Double -> Bool)

twice f = f . f
fourTimes = twice . twice

capitalizeWord [] = []
capitalizeWord [x] = [toUpper x]
capitalizeWord (x:xs) = (toUpper x) : xs


allEq (x:xs)   = all (== x)  xs
isIdemp f x = allEq [f x, twice f x, fourTimes f x]
isCapitalizeWordIdemp  = isIdemp capitalizeWord
isSortIdemp  = isIdemp sort

doesSortIdemp = quickCheck (isSortIdemp :: [Int] -> Bool)
doesCapIdem = quickCheck (isCapitalizeWordIdemp :: String -> Bool)


data Fool = Fulse | Frue deriving (Show, Eq)

eqFoolGen = elements [Fulse, Frue]
freqFoolGen = frequency [(1, return Fulse), (2, return Frue)]
