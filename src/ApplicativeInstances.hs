module ApplicativeInstances where

import Data.Monoid


newtype Identity a = Identity a deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity x) = Identity (f x)


instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x) 
  



-- Constant Instance of Applicative

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty  
  (<*>) (Constant x) (Constant y) = Constant (x <> y)




-- List Instance of Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

  
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)
    where
      append Nil ys = ys
      append xs Nil = xs
      append (Cons x xs) ys = Cons x (append xs ys)




-- ZipList Instance of Applicative

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show)

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList $ repeat mempty
  mappend (ZipList xs) (ZipList ys) = ZipList (zipWith mappend xs ys) 


instance Functor ZipList where
  fmap f zs = ZipList (fmap f $ getZipList zs)


instance Applicative ZipList where
  pure x = ZipList (repeat x)
  (<*>) (ZipList fs) (ZipList xs) = ZipList (zipWith (\f x -> f x) fs xs)


-- Validation Instance of Aplicative

data Validation' err a = Failure' err | Success' a deriving (Eq, Show)


instance Functor (Validation' err) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' v) = Success' (f v)


instance Monoid e => Applicative (Validation' e) where
  pure = Success'
  (<*>) (Failure' e1) (Failure' e2) = Failure' (e1 <> e2)
  (<*>) (Failure' e) _ = Failure' e
  (<*>) _ (Failure' e) = Failure' e
  (<*>) (Success' f) (Success' v) = Success' (f v) 


  
