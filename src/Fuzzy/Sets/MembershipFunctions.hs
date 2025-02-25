{- | This module contains parametrized membership function 
    use currying to construct the functions 
    arguments a b c ... are parameters for constructing specific functions 
    x is the variable for which membership is evaluated
-}

module Fuzzy.Sets.MembershipFunctions(
    constant, 
    linear, 
    sigmoid, 
    triangular,
    rectangular,
    trapezoid,
    gaussian,
    exponential
) where
    

import Lattices.ResiduatedLattice


-- | Constant function returns the same value of x for each y 
constant :: ResiduatedLattice l => Double -> (Double -> l)
constant x y = mkLattice x


{-| Standart textbook linear function where f(x) = a * x + b

>           / 
>          /   
>         /

-}
linear :: ResiduatedLattice l => Double -> Double -> (Double -> l)
linear a b x =
    let y = a * x + b
    in mkLattice y


{-| Standart logistic function

>                                  
>                       ______
>                    .´      
>                  /           
>           _____.'       
      
-}                    
sigmoid :: ResiduatedLattice l => Double -> Double -> (Double -> l)
sigmoid k x0 x =
    let pow = -k * (x - x0)
    in mkLattice $ 1 / (1 + (exp 1 ** pow))

{-| A combination of two linear functions
-- with this specific shape.

>          
>             /\
>          __/  \__

-}
triangular :: ResiduatedLattice l => Double -> Double -> (Double -> l)
triangular a b x = 
    let y = 1 - abs (x - a) + b
    in mkLattice y


{-| Constant function on interval [a, b] else returns 'bot' of Residuated lattice
-- this creates a rectangle shaped function

>             _____ 
>            |     |
>         ___|     |___

-}
rectangular :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
rectangular a b h x
    | x >= a && x <= b = mkLattice h
    | otherwise        = bot


{-| Trapezoid function is combination of triangular and rectangular functions

>           _______   
>          /       \
>       __/         \__

-}
trapezoid :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
trapezoid a b c x 
    | x <= a || x >= c = bot
    | x >= a && x <= b = mkLattice ((x - a) / (b - a))
    | x >= b && x <= c = mkLattice ((c - x) / (c - b))


{-| Gausian function, also called Bell Curve

>           
>                     .-' `-.
>                   .'       `.
>                  /           \
>                /               \
>        ______.'                 '.____

-}
gaussian :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
gaussian a b c x =
    let e     = exp 1
        numer = (x + b) ** 2
        denom = 2 * (c ** 2)
        pow   = -(numer / denom)
    in mkLattice $ a * e**pow


{-| Exponential function eˣ

>           
>                    |
>                   '  
>                  /   
>                 /     
>        ______.'   

-}
exponential :: ResiduatedLattice l => Double -> l
exponential x = 
    let e = exp 1
    in mkLattice $ e**x