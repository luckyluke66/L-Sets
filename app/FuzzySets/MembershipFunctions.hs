module FuzzySets.MembershipFunctions(
    constant, 
    linear, 
    sigmoid, 
    triangular,
    rectangular,
    trapezoid,
    gaussian
) where
    
import Lattices.UnitInterval
import Lattices.ResiduatedLattice

-- use currying to construct functions 
-- arguments a b c ... are constants for constructing specific functions 
-- x is the variable for which membership is evaluated

constant :: ResiduatedLattice l => Double -> (Double -> l)
constant x y = mkLattice x

linear :: ResiduatedLattice l => Double -> Double -> (Double -> l)
linear a b x =
    let y = a * x + b
    in mkLattice y

sigmoid :: ResiduatedLattice l => Double -> Double -> (Double -> l)
sigmoid k b x =
    let pow = -k * (x - b)
    in mkLattice $ 1 / (1 + (exp 1 ** pow))

triangular :: ResiduatedLattice l => Double -> Double -> (Double -> l)
triangular a b x = 
    let y = 1 - abs (x - a) + b
    in mkLattice y

rectangular :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
rectangular a b h x
    | x >= a && x <= b = mkLattice h
    | otherwise        = bot

trapezoid :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
trapezoid a b c x 
    | x <= a || x >= c = bot
    | x >= a && x <= b = mkLattice ((x - a) / (b - a))
    | x >= b && x <= c = mkLattice ((c - x) / (c - b))

gaussian :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
gaussian a b c x =
    let e     = exp 1
        numer = (x + b) ** 2
        denom = 2 * (c ** 2)
        pow   = -(numer / denom)
    in mkLattice $ a * e**pow