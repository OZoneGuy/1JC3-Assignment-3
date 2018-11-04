{- Assignment 3
 - Name: Omar Alkersh
 - Date: 23/10/18
 -}
module Assign_3 where

macid :: String
macid = "alkersho"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Inserts the Num n into the polynomial (Poly a) and returns
 - Num value
 - Uses recursion when the Poly a is wither Sum or Prod with the base cases being X or Coef
 - eg -> polyValue (Prod (Prod X X) (Coef 5)) 5
 - insert 5 into Prod (Prod X X) (Coef 5) => (Prod X X) * Coef 5 =>
 - X * X * 5 => (5)*(5)*5 => 125
 -}

polyValue :: Num a => Poly a -> a -> a
polyValue X n = n
polyValue (Coef x) _ = x
polyValue (Sum x y) n = polyValue x n + polyValue y n
polyValue (Prod x y) n = polyValue x n * polyValue y n

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description: Uses recursion to find the highest exponent(degree) in the polynomial
 - with the base cases being X => 1 and Coef _ => 0
 - Prod x y -> add the degree of x and y
 - Sum x y -> find the highest degree between x and y
 - eg.
 - The polynomial Prod X (Sum (Coef 2) X) => x(2+x) = 2x+x^2
 - The high exponent is 2
 -}

polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree X = 1
polyDegree (Coef _) = 0
polyDegree (Sum x y) = max(polyDegree x) (polyDegree y)
polyDegree (Prod x y) = polyDegree x + polyDegree y

{- -----------------------------------------------------------------
 - polyDeriv
 - -----------------------------------------------------------------
 - Description: finds the derivative of the polynomial
 - eg.
 - Prod X (Prod x (Sum X (Coef 2))) => x(x(x+2)) = x^3+2x^2
 - If the Poly is just X then the derivative is 1
 - If the Poly is a constant/Coef a then the derivative is 0
 - If the Poly is Sum a b then the derivative is the sum of
 derivative of a and the derivativeof b
 - If the Poly is a Prod then I will be using the product rule:
 d/dx x(x+2) = x*(d/dx(x+2)) + d/dx(x)(x+2)
 -}

polyDeriv :: Num a => Poly a -> Poly a
polyDeriv X = Coef 1
polyDeriv (Coef _) = Coef 0
polyDeriv (Sum x y) = Sum (polyDeriv x) (polyDeriv y)
polyDeriv (Prod x y) = Sum (Prod x (polyDeriv y)) (Prod (polyDeriv x) y)

{- -----------------------------------------------------------------
  - Test Cases
  - -----------------------------------------------------------------
  - polyValue
  - -----------------------------------------------------------------
  - - Function: polyValue
  - - Test Case Number: 1-A
  - - Input: X n where n is a Num
  - - Expected Output: n
  - - Acutal Output: n
  - -----------------------------------------------------------------
  - - Function: polyValue
  - - Test Case Number: 1-B
  - - Input: (Coef n) N
  - - Expected Output: n
  - - Acutal Output: n
  - -----------------------------------------------------------------
  - - Function: polyValue
  - - Test Case Number: 1-C
  - - Input: Sum(X X) n
  - - Expected Output: 2n
  - - Acutal Output: 2n
  - -----------------------------------------------------------------
  - - Function: polyValue
  - - Test Case Number: 1-D
  - - Input: Prod(X X) n
  - - Expected Output: n^2
  - - Acutal Output: n^2
  - -----------------------------------------------------------------
  - - Function: polyValue
  - - Test Case Number: 1-E
  - - Input: Prod(X (Sum X (Coef 3))) n
  - - Expected Output: n^2 + 3n
  - - Acutal Output: n^2 + 3n
  - -----------------------------------------------------------------
  -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 1-A
 - - Input: X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 1-B
 - - Input: (Coef n)
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 1-C
 - - Input: Sum(X X)
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 1-D
 - - Input: Prod(X X)
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 1-E
 - - Input: Prod(X (Sum X (Coef 3)))
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
  - Test Cases
  - -----------------------------------------------------------------
  - polyDeriv
  - -----------------------------------------------------------------
  - - Function: polyDeriv
  - - Test Case Number: 1-A
  - - Input: X
  - - Expected Output: Coef 1
  - - Acutal Output: Coef 1
  - -----------------------------------------------------------------
  - - Function: polyDeriv
  - - Test Case Number: 1-B
  - - Input: (Coef n)
  - - Expected Output: Coef 0
  - - Acutal Output: Coef 0
  - -----------------------------------------------------------------
  - - Function: polyDeriv
  - - Test Case Number: 1-C
  - - Input: Sum(X X)
  - - Expected Output: Coef 2
  - - Acutal Output: Sum (Coef 1) (Coef 1)
  - -----------------------------------------------------------------
  - - Function: polyDeriv
  - - Test Case Number: 1-D
  - - Input: Prod(X X)
  - - Expected Output: Prod X (Coef 2)
  - - Acutal Output: Sum (Prod X (Coef 1)) (Prod (Coef 1) X)
  - -----------------------------------------------------------------
  - - Function: polyDeriv
  - - Test Case Number: 1-E
  - - Input: Prod(X (Sum X (Coef 3)))
  - - Expected Output: Sum (Prod X (Coef 2)) ((Coef 3) )
  - - Acutal Output: Sum (Prod X (Sum (Coef 1) (Coef 0))) (Prod (Coef 1) (Sum X (Coef 3)))
  - -----------------------------------------------------------------
  -}
