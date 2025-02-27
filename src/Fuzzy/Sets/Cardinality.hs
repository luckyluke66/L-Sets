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
sigmaCount :: (FuzzySet set a l) => set -> Double
sigmaCount set = sum [realToFrac (f x) | x <- universe set]
    where f = member set 


-- | Similar to 'sigmaCount' 
-- before summing we apply a modifier function c.
-- |A| = Σ c(A(u)) for all u ∈ U
sigmaCountWithModifier :: (FuzzySet set a l) => (l -> l) -> set -> Double
sigmaCountWithModifier c set = sum [realToFrac $ (c . f) x | x <- universe set]
    where f = member set


-- | Sigma count with applied threshold.
-- Only values that are greater then threshold are summed.
thresholdSigmaCount :: (FuzzySet set a l) => l -> set -> Double
thresholdSigmaCount threshold set = 
    sum [realToFrac (f x) | x <- universe set, f x >= threshold]
    where f = member set
    

-- | Normalized sigma count is like standart sigma count, but the value is the normalized to be in interval [0,1].
normalizedSigmaCount :: (FuzzySet set a l) => set -> l
normalizedSigmaCount set = mkLattice $ sigmaCount set / fromIntegral (length $ universe set)


-- | Modifier function is parametrized function, this function can be used as a template to create a specific modifier function.
-- some of the more specific functions follow
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