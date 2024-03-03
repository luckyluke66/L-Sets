module MembershipFunctions(
    constant, 
    linear, 
    sigmoid, 
    triangular,
    rectangular,
    trapezoid,
    gaussian
) where

import FuzzySet(L)
 
-- use curring to construct functions 
-- arguments a b c ... are constants for constructing specific functions 
-- x is the variable for which membership is evaluated

ensureBounds :: RealFloat a => a -> a
ensureBounds x = max 0 (min 1 x)

constant :: RealFloat a => a -> a
constant = ensureBounds

linear :: RealFloat a => a -> a -> a -> a 
linear a b x =
    let y = a * x + b
    in ensureBounds y

sigmoid :: RealFloat a => a -> a -> a -> a -> a
sigmoid a b c x =
    let pow = -(a * x + b)
    in  1 / (1 + (c ** pow))

triangular :: RealFloat a => a -> a -> a -> a
triangular a b x = 
    let y = 1 - abs (x - a) + b
    in ensureBounds y

rectangular :: RealFloat a => a -> a -> a -> a -> a
rectangular a b h x
    | x >= a && x <= b = ensureBounds h
    | otherwise        = 0  

trapezoid :: RealFloat a => a -> a -> a -> a -> a
trapezoid a b c x 
    | x <= a || x >= c = 0
    | x >= a && x <= b = ensureBounds ((x - a) / (b - a))
    | x >= b && x <= c = ensureBounds ((c - x) / (c - b))

gaussian :: RealFloat a => a -> a -> a -> a -> a
gaussian a b c x =
    let e     = exp 1
        numer = (x + b) ** 2
        denom = 2 * (c ** 2)
        pow   = -(numer / denom)
    in a * e**pow