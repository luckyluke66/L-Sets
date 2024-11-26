module Properties (
    isEmpty,
    isCrisp,
    isUniversal,
    gradedSubsethood,
    gradedEquality,
) where

isEmpty :: (ResiduatedLattice l) => FuzzySet -> l
isEmpty (FuzzySet f u)
    | x == top = top
    | otherwise = bot 
    where  = foldr (/\) top [f x | x <- u]

isCrisp :: (ResiduatedLattice l) => FuzzySet -> l
isCrisp (FuzzySet f u)
    | crisp = top
    | otherwise = bot 
    where crisp = all (\x -> x == top || x == bot) [f x | x <- u]

isUniversal :: (ResiduatedLattice l) => FuzzySet -> l
isUniversal (FuzzySet f u)
    | universal = top
    | otherwise = bot 
    where universal = all (== top) [f x | x <- u]

gradedSubsethood :: ResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedSubsethood = gradedOperation (-->)

gradedEquality :: ResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: ResiduatedLattice l => (l -> l -> l) -> FuzzySet a l -> FuzzySet a l -> l
gradedOperation op (FuzzySet f u) (FuzzySet g _) =
    foldr (/\) bot $ zipWith op (map f u) (map g u)