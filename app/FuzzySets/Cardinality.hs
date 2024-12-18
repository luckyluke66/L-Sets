module FuzzySets.Cardinality(
    sigmaCount,
    thresholdSigmaCount,
    normalizedSigmaCount,
    sigmaCountWithModifier,
    modifierFunction,
    sigmoidModifier,
    identityModifier,
    subDiagonalModifier,
    alphaCutModifier
) where 

import Lattices.ResiduatedLattice
import FuzzySets.FuzzySet


sigmaCount :: (ResiduatedLattice l) => FuzzySet a l -> Double
sigmaCount (FuzzySet f universe) = sum [realToFrac (f x) | x <- universe]

sigmaCountWithModifier :: (ResiduatedLattice l) => (l -> l) -> FuzzySet a l -> Double
sigmaCountWithModifier c (FuzzySet f universe) = 
    sum [realToFrac $ (c . f) x | x <- universe]

thresholdSigmaCount :: (ResiduatedLattice l) => l -> FuzzySet a l -> Double
thresholdSigmaCount threshold (FuzzySet f universe) = 
    sum [realToFrac (f x) | x <- universe, f x >= threshold]

normalizedSigmaCount :: (ResiduatedLattice l) => FuzzySet a l -> l
normalizedSigmaCount fuzzySet@(FuzzySet _ universe) = mkLattice $ sigmaCount fuzzySet / fromIntegral (length universe)

-- modifier functions 

modifierFunction :: (ResiduatedLattice l) => Double -> Double -> Double -> (l -> l)
modifierFunction p r threshold a
    | realToFrac a < threshold = mkLattice $ threshold ** (1 - p) * realToFrac a ** p
    | realToFrac a >= threshold = mkLattice $ 1 - (1 - threshold) ** (1 - r) * (1 - realToFrac a) ** r

sigmoidModifier :: (ResiduatedLattice l) => Double -> Double -> (l -> l)
sigmoidModifier p = modifierFunction p p

identityModifier :: (ResiduatedLattice l) => (l -> l)
identityModifier = modifierFunction 1 1 1

subDiagonalModifier :: (ResiduatedLattice l) => Double -> (l -> l)
subDiagonalModifier p = modifierFunction p 1 1

alphaCutModifier :: (ResiduatedLattice l) => Double -> (l -> l)
alphaCutModifier threshold a 
    | realToFrac a <= threshold = bot
    | realToFrac a > threshold = top