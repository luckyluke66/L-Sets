module FuzzySet(
    FuzzySet(FuzzySet),
    membership,
    crop
    ) where

newtype RealFloat a => FuzzySet a = FuzzySet (a -> a)

membership :: RealFloat a => FuzzySet a -> a -> a
membership (FuzzySet f) = f 

crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)

union :: RealFloat a => FuzzySet a -> FuzzySet a -> FuzzySet a
union (FuzzySet x) (FuzzySet y) = FuzzySet (\z -> max (x z) (y z))

intersection :: RealFloat a => FuzzySet a -> FuzzySet a -> FuzzySet a
intersection (FuzzySet x) (FuzzySet y) = FuzzySet (\z -> min (x z) (y z))