module FuzzySets.Cardinality(
    sigmaCount,
    thresholdSigmaCount,
    normalizedSigmaCount
) where 

import Lattices.CompleteResiduatedLattice
import FuzzySets.FuzzySet


sigmaCount :: (CompleteResiduatedLattice l) => FuzzySet a l -> Double
sigmaCount (FuzzySet f universe) = foldr (+) 0.0 [realToFrac (f x) | x <- universe]


thresholdSigmaCount :: (CompleteResiduatedLattice l) => l -> FuzzySet a l -> Double
thresholdSigmaCount threshold (FuzzySet f universe) = 
    foldr (+) 0.0 [realToFrac (f x) | x <- universe, f x >= threshold]

normalizedSigmaCount :: (CompleteResiduatedLattice l) => FuzzySet a l -> l
normalizedSigmaCount fuzzySet@(FuzzySet _ universe) = mkLattice $ (sigmaCount fuzzySet) / fromIntegral (length universe)
