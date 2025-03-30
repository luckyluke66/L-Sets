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
    trapezoidal,
    gaussian,
    exponential,
) where


import Lattices.ResiduatedLattice


-- | Constant function return a for any value of x \[f(x) = a\]
constant :: ResiduatedLattice l => Double -> (Double -> l)
constant a _ = mkLattice a


{-| Standart textbook linear function where \[f(x) = ax + b\]

>           / 
>          /   
>         /

-}
linear :: ResiduatedLattice l => Double -> Double -> (Double -> l)
linear a b x =
    let y = a * x + b
    in mkLattice y


{-| Standart logistic function
Takes K which is growth value of the function and x0 a midpoint of the function. \[f(x) = \frac{1}{1 + e^{ -k(x - x0)}} \]
               
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
with this specific shape. first and second arguments are interval determining where the triangle will be on the number line. \[
\operatorname{tri}(x) =
\begin{cases}
    \frac{x - a}{b - a}, & a \leq x < b \\
    \frac{c - x}{c- b}, & b \leq x \leq c \\
    0, & \text{otherwise}
\end{cases}
\] 0 stands for 'bot' 

>          
>             /\
>          __/  \__

-}
triangular :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
triangular a b c x 
    | a <= x && x < b = mkLattice $ (x - a) / (b - a)
    | b <= x && x < c = mkLattice $ (c - x) / (c - b)
    | otherwise       = bot
    


{-| Constant function on interval [a, b], first two arguments, else returns 'bot' of Residuated lattice
- this creates a rectangle shaped function. Third argument is height of the set.

>             _____ 
>            |     |
>         ___|     |___

-}
rectangular :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
rectangular a b h x
    | x >= a && x <= b = mkLattice h
    | otherwise        = bot


{-| Trapezoidal function is combination of triangular and rectangular functions \[
\operatorname{tra}(x) =
\begin{cases}
    \frac{x - a}{b1 - a}, & a \leq x < b1 \\
    1, & b1 \leq x < b2  \\
    \frac{c - x}{c -b2}, & b2 \leq x < c \\
    0, & \text{otherwise}
\end{cases}
\]

>           _______   
>          /       \
>       __/         \__

-}
trapezoidal :: ResiduatedLattice l => Double -> Double -> Double -> Double -> (Double -> l)
trapezoidal a b1 b2 c x 
    | a <= x && x < b1   = mkLattice $ (x - a) / (b1 - a)
    | b1 <= x && x <= b2 = top
    | b2 <= x && x < c   = mkLattice $ (c - x) / (c - b2)
    | otherwise = bot


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