{- | This module contains various compositions of binary 'LRelations'.
we provide natural language description of meaning of each type of composition 
on simple example with two relations R (x , y) - patient x has symptom y 
and S (y, z) - y is a symptom of z.
-}
module Fuzzy.Relations.RelationComposition (
    circlet,
    subproduct,
    superproduct,
    square,
) where

import Fuzzy.Relations.LRelation
import Lattices.ResiduatedLattice
import Utils.Utils

-- | R 'circlet' S (x, z) - Truth degree to which there is symptom y,
-- such that patient x has symptom y and y is a symptom of disease z
circlet :: (Eq a,ResiduatedLattice l) =>  LRelation a l -> LRelation a l -> LRelation a l
circlet (LRelation r u) (LRelation s _ ) = LRelation composition u
    where composition (x, z) = foldr (\/) bot [r (x, y) `tnorm` s (y, z) | y <- universe]
          universe = universeToList u

-- | R 'subproduct' S (x, z) - truth degree to which it is true that 
-- if patient x has symptom y than y is symptom of z
subproduct :: (Eq a,ResiduatedLattice l) => LRelation a l -> LRelation a l -> LRelation a l
subproduct (LRelation r u) (LRelation s _) = LRelation composition u
    where composition (x, z) = foldr (/\) top [r (x, y) --> s (y, z) | y <- universe]
          universe = universeToList u
        
-- | R 'superproduct' S (x, z) - truth degree to which it is true that 
-- patient x has all symptoms of z
superproduct :: (Eq a,ResiduatedLattice l) => LRelation a l -> LRelation a l -> LRelation a l
superproduct (LRelation r u) (LRelation s _) = LRelation composition u
    where composition (x, z) = foldr (/\) bot [r (x, y) <-- s (y, z) | y <- universe]
          eq x y = r (x, y) <--> s (x, y)
          universe = universeToList u


-- | R 'square' S (x, z) - truth degree to which it is true that patient x has exactly the symptoms of z
square :: (Eq a, ResiduatedLattice l) => LRelation a l -> LRelation a l -> LRelation a l
square (LRelation r u) (LRelation s _) = LRelation composition u
    where composition (x, z) = foldr (/\) bot [r (x, y) <--> s (y, z) | y <- universe]
          universe = universeToList u

