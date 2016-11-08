import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Test 
import ApplicativeInstances 
import Data.Monoid

-- Identity Instances of Arbitrary and EqProp 

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- Constant Instances of Arbitrary and EqProp

instance (Arbitrary a, Monoid a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


-- List Instances of Arbitrary and EqProp

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = sized $ \n -> do
    k <- choose (0, n)
    genList k Nil
    where
      genList :: Arbitrary a => Int -> List a -> Gen (List a)
      genList i xs
        | i <= 0 = return xs
        | otherwise = do
            a <- arbitrary
            genList (i - 1) (Cons a xs)

instance Eq a => EqProp (List a) where
  (=-=) = eq
            

-- ZipList Instances of Arbitrary and EqProp

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

{-
  Note that an ordinary eq is not enough to test ZipList due to the meaning of pure.
  That is, the law of homomorphism would chase infinite lists forever if we just
  use eq. That's why we limit the number of elements to be checked for equality in
  a list to 3000.
-}
instance Eq a => EqProp (ZipList a) where
  (=-=) (ZipList xs) (ZipList ys) = eq (take 3000 xs) (take 3000 ys)


-- Validation Instances of Arbitrary and EqProp
instance (Monoid err, Arbitrary err, Arbitrary a) => Arbitrary (Validation' err a) where
  arbitrary = do
    err <- arbitrary
    a <- arbitrary
    elements [Failure' err, Success' a]
    
instance (Eq err, Monoid err, Eq a) => EqProp (Validation' err a) where
  (=-=) = eq 



type Dummy1 = (String, [Int], Int)
type Dummy2 = (Char, Int, (Float, Float))

main :: IO ()
main = do
  putStrLn "Starting Tests"
  putStrLn " "
  putStrLn "Indentity Applicative"
  quickBatch (applicative $ Identity ("Haskell", [(1 :: Int)], (1 :: Int)))
  putStrLn " "
  putStrLn "Constant Applicative"
  quickBatch (applicative $ (Constant "Haskell" :: Constant String Dummy1))
  putStrLn " "
  let dummyVal :: Dummy2
      dummyVal = ('a', 1, (1.0, 1.0))
  putStrLn "List Applicative"
  -- Note that for the List Applicative we have to set a small number of tests
  -- so that our memory is not bloated.
  let args1 = Args Nothing 61 1 61 True
  checkBatch args1
            (applicative $ (Cons dummyVal Nil))
  -- For ZipList we'll test both Monoids and Applicatives          
  putStrLn " "
  putStrLn "ZipList Monoid & Applicative"
  let args2 = Args Nothing 100 1 100 True
  checkBatch args2 (monoid $ ZipList [Sum (0 :: Int)])
  checkBatch args2
             (applicative $ ZipList [dummyVal])
  putStrLn " "
  putStrLn "Validation Applicative"
  quickBatch (applicative (Success' dummyVal :: Validation' [String] Dummy2)) 
  
