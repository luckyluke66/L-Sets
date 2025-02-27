{-# LANGUAGE FunctionalDependencies #-}

module FuzzySet(
    FuzzySet(..),
    alphaCut,
    union,
    intersection,
    complement,
    setTnorm,
    setResiduum
) where

import Lattices.ResiduatedLattice

-- | Type class defines the basic behavior for a fuzzy set
class (ResiduatedLattice l) => FuzzySet set a l | set -> a l where
    -- | membership function 
    member :: set -> a -> l
    universe :: set -> [a]
    universeCardinality :: set -> Int
    universeCardinality s = length $ universe s
    elements :: set -> [l]
    elements s = map (member s) (universe s)
    mkFuzzySet :: (a -> l) -> [a] -> set 


alphaCut :: (ResiduatedLattice l, FuzzySet set a l) => l -> set -> [a]
alphaCut alpha set = [x | x <- u, f x >= alpha]
    where f = member set
          u = universe set


-- | Fuzzy set union A ∪ B = C
union :: (ResiduatedLattice l, FuzzySet set a l) => set -> set -> set
union set1 set2 = mkFuzzySet (\x -> f x \/ g x) u
    where f = member set1
          g = member set2
          u = universe set1


-- | Fuzzy set intersection A ∩ B
intersection :: (ResiduatedLattice l, FuzzySet set a l) => set -> set -> set
intersection set1 set2 = mkFuzzySet (\x ->  f x /\ g x) u
    where f = member set1
          g = member set2
          u = universe set1


-- | Fuzzy set complement A′ 
complement :: (ResiduatedLattice l, FuzzySet set a l) => set -> set
complement set = mkFuzzySet (negation . f)  u
    where f = member set
          u = universe set


-- | 'tnorm' over fuzzy sets 
setTnorm :: (ResiduatedLattice l, FuzzySet set a l) => set -> set -> set
setTnorm set1 set2 = mkFuzzySet (\x -> f x `tnorm` g x) u
    where f = member set1
          g = member set2
          u = universe set1


-- | Residuum ('-->') over fuzzy sets
setResiduum :: (ResiduatedLattice l, FuzzySet set a l) => set -> set -> set
setResiduum set1 set2 = mkFuzzySet (\x -> f x --> g x) u
    where f = member set1
          g = member set2
          u = universe set1
