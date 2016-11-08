# Testing Applicative Instances With QuickCheck

Describing functor properties to test them is easy with the aid
of Test.QuickCheck.Function. However, the Applicative class
has more complex laws which would be rather cumbersome to
test just with the bare utilities of QuickCheck. That's why in
this example I'm using Test.QuickCheck.Checkers to easily test
Applicative instances by just providing Arbitrary and EqProp
instances of the data types I want to check.