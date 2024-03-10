module MembershipFunctions(
    constant, 
    linear, 
    sigmoid, 
    triangular,
    rectangular,
    trapezoid,
    gaussian
) where
import Lattices.UnitInterval (mkUnitInterval, UnitInterval)

-- use curring to construct functions 
-- arguments a b c ... are constants for constructing specific functions 
-- x is the variable for which membership is evaluated

ensureBounds :: RealFloat a => a -> a
ensureBounds x = max 0 (min 1 x)

constant :: Double -> Double -> UnitInterval
constant x y = mkUnitInterval x

linear :: Double -> Double -> Double -> UnitInterval 
linear a b x =
    let y = a * x + b
    in mkUnitInterval y

sigmoid :: Double -> Double -> Double -> Double -> UnitInterval
sigmoid a b c x =
    let pow = -(a * x + b)
    in mkUnitInterval $ 1 / (1 + (c ** pow))

triangular :: Double -> Double -> Double -> UnitInterval
triangular a b x = 
    let y = 1 - abs (x - a) + b
    in mkUnitInterval y

rectangular :: Double -> Double -> Double -> Double -> UnitInterval
rectangular a b h x
    | x >= a && x <= b = mkUnitInterval h
    | otherwise        = 0  

trapezoid :: Double -> Double -> Double -> Double -> UnitInterval
trapezoid a b c x 
    | x <= a || x >= c = 0
    | x >= a && x <= b = mkUnitInterval ((x - a) / (b - a))
    | x >= b && x <= c = mkUnitInterval ((c - x) / (c - b))

gaussian :: Double -> Double -> Double -> Double -> UnitInterval
gaussian a b c x =
    let e     = exp 1
        numer = (x + b) ** 2
        denom = 2 * (c ** 2)
        pow   = -(numer / denom)
    in mkUnitInterval $ a * e**pow