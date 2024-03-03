module FuzzySet(
    FuzzySet(FuzzySet),
    CompleteResiduatedLattice,
    L,
    membership,
    crop
    ) where


newtype RealFloat a => FuzzySet a = FuzzySet (a -> a)

class Ord a => CompleteResiduatedLattice a where
    (/\), (\/), residuum, multipl  :: a -> a -> a
    inf, sup :: a


instance CompleteResiduatedLattice Double where 
    (/\) = min
    (\/) = max
    inf = 0
    sup = 1
    residuum a b = min (1 - a + b) 1
    multipl a b = max (a + b - 1) 0

instance CompleteResiduatedLattice Bool where
    (/\) = min
    (\/) = max
    inf = False
    sup = True
    residuum = implication 
    multipl a b = a && b

implication :: Bool -> Bool -> Bool
implication True False = False
implication _ _        = True
  
membership :: RealFloat a => FuzzySet a -> a -> a
membership (FuzzySet f) = f

crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)

union :: RealFloat a => FuzzySet a -> FuzzySet a -> FuzzySet a
union (FuzzySet x) (FuzzySet y) = FuzzySet (\z -> max (x z) (y z))

intersection :: RealFloat a => FuzzySet a -> FuzzySet a -> FuzzySet a
intersection (FuzzySet x) (FuzzySet y) = FuzzySet (\z -> min (x z) (y z))

complement :: RealFloat a => FuzzySet a -> FuzzySet a
complement (FuzzySet f) = FuzzySet (\x -> 1 - f x)