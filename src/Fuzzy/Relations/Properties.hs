-- | This module contains functions that determine degree of properties of a 'LRelation'
module Fuzzy.Relations.Properties (
    ref,
    sym,
    tra,
    irref,
    asym,
) where

import Fuzzy.Relations.LRelation
import Lattices.ResiduatedLattice
import Utils.Utils(universeToList)
import Lattices.UnitIntervalStructures.Godel

-- functions determining degree of properties of fuzzy relations


{- | Degree to which 'LRelation' is reflexive.

An 'LRelation' is reflexive if \(ref \: rel =\) 'top'

==== __Examples__

>>> let u = [(1, 1), (2, 2), (3, 3)]
>>> let rel = LRelation (\(x, y) -> if x == y then top else bot) u :: LRelation Int UIGodel
>>> ref rel
1.0

>>> let rel = LRelation (\(x, y) -> bot) u :: LRelation Int UIGodel
>>> ref rel
0.0
-}
ref :: (Eq a, ResiduatedLattice l) => LRelation a l -> l
ref (LRelation f u) =
    foldr (/\) top [f x | x <- u,  uncurry (==) x]
    where universe = universeToList u


{- | Degree to which 'LRelation' is symmetric.

An 'LRelation' is symmetric if \(sym \: rel =\) 'top'

==== __Examples__

>>> let u = [(1, 2), (2, 1), (2, 3), (3, 2)]
>>> let rel = LRelation (const  0.5) u :: LRelation Int UILukasiewicz
>>> sym rel
1.0

>>> let rel = LRelation (\(x, y) -> if x < y then 0.7 else 0.3) u :: LRelation Int UILukasiewicz
>>> sym rel
0.6
-}
sym :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
sym (LRelation f u) =
    foldr (/\) top [f (x, y) --> f (y, x) | x <- universe, y <- universe]
    where universe = universeToList u


{- | Degree to which 'LRelation' is transitive.

An 'LRelation' is transitive if \(tra \: rel =\) 'top'

==== __Examples__

>>> let u = [(1, 2), (2, 3), (1, 3)]
>>> let rel = LRelation (\(x, y) -> if x < y then 0.7 else 0.3) u :: LRelation Int UILukasiewicz
>>> tra rel
1.0

>>> let rel = LRelation (\(x, y) -> if x == y then 0.2 else 0.7) u :: LRelation Int UILukasiewicz
>>> tra rel
0.5
-}
tra :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
tra (LRelation f u) =
    foldr (/\) top [f (x, y) /\ f (y, z) --> f (x, z) |
                      x <- universe, y <- universe, z <- universe]
    where universe = universeToList u


{- | Degree to which 'LRelation' is irreflexive.

An 'LRelation' is irreflexive if \(irref \: rel =\) 'top'

==== __Examples__

>>> let u = [(1, 1), (2, 2), (3, 3)]
>>> let rel = LRelation (\(x, y) -> if x == y then bot else top) u :: LRelation Int UILukasiewicz
>>> irref rel
1.0

>>> let rel = LRelation (\(x, y) -> if x == y then 0.7 else bot) u :: LRelation Int UILukasiewicz
>>> irref rel
0.3
-}
irref :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
irref (LRelation f u) =
    foldr (/\) top [negation (f x) | x <- u,  uncurry (==) x]
    where universe = universeToList u


{- | Degree to which 'LRelation' is asymmetric.

An 'LRelation' is asymmetric if \(asym \: rel =\) 'top'

==== __Examples__

>>> let u = [(1, 2), (2, 3), (3, 1)]
>>> let rel = LRelation (\(x, y) -> if x < y then 0.7 else bot) u :: LRelation Int UILukasiewicz
>>> asym rel
1.0
-}
asym :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
asym (LRelation f u) =
    foldr (/\) top [f (x, y) --> negation (f (y, x)) | x <- universe, y <- universe]
    where universe = universeToList u
