module Fuzzy.Sets.Cardinality(
    sigmaCount,
    thresholdSigmaCount,
    normalizedSigmaCount,
    sigmaCountWithModifier,
    -- * Modifier functions
    modifierFunction,
    sigmoidModifier,
    identityModifier,
    subDiagonalModifier,
    alphaCutModifier
) where 

import Lattices.ResiduatedLattice
import Fuzzy.Sets.LSet

-- | Most commonly used way to tell size of a fuzzy set
-- Sum of membership values of all elements from universe set 
-- For fuzzy set a |A| = Σ A(u) for all u ∈ U
sigmaCount :: (ResiduatedLattice l) => LSet a l -> Double
sigmaCount (LSet f universe) = sum [realToFrac (f x) | x <- universe]

-- | Similar to 'sigmaCount' 
-- before summing we apply a modifier function c.
-- |A| = Σ c(A(u)) for all u ∈ U
sigmaCountWithModifier :: (ResiduatedLattice l) => (l -> l) -> LSet a l -> Double
sigmaCountWithModifier c (LSet f universe) = 
    sum [realToFrac $ (c . f) x | x <- universe]

-- | Sigma count with applied threshold.
-- Only values that are greater then threshold are summed.
thresholdSigmaCount :: (ResiduatedLattice l) => l -> LSet a l -> Double
thresholdSigmaCount threshold (LSet f universe) = 
    sum [realToFrac (f x) | x <- universe, f x >= threshold]

-- | Normalized sigma count is like standart sigma count, but the value is the normalized to be in interval [0,1].
normalizedSigmaCount :: (ResiduatedLattice l) => LSet a l -> l
normalizedSigmaCount fuzzySet@(LSet _ universe) = mkLattice $ sigmaCount fuzzySet / fromIntegral (length universe)

-- | Modifier function is parametrized function, this function can be used as a template to create a specific modifier function.
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

{- $modifier functions 
Modifier functions give us a way to shift sigma count in case where needed. 
Common solution for problem of accumulation of large number of small values. 
-}