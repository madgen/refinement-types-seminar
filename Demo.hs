module Demo where

import Prelude hiding (head, take, gcd, pred)

infixr 6 :::
data List a = Nil | (:::) a (List a)

instance Show a => Show (List a) where
  show Nil = "Nil"
  show (x ::: xs) = show x ++ " ::: " ++ show xs

{-@ measure len' @-}
len' :: List a -> Int
len' Nil = 0
len' (_ ::: xs) = 1 + len' xs














--------------------------------------------------------------------------------
-- Milner lied
--------------------------------------------------------------------------------

-- "Well-typed programs don't go wrong" - Robin Milner

  {-
head :: List a -> a
head (x ::: _) = x
head Nil = error "I wasn't supposed to go wrong!"

headEx1 :: Int
headEx1 = head (1 ::: 2 ::: 3 ::: Nil)

headEx2 :: Int
headEx2 = head Nil
  -}





































--------------------------------------------------------------------------------
-- Constraints on natural numbers
--------------------------------------------------------------------------------

  {-
take :: Int -> List a -> List a
take 0 _   = Nil
take _ Nil = Nil
take n (x ::: xs) =
  if n > 0
    then x ::: take (n - 1) xs
    else error "Negative number of elements to take"

takeEx1 = take 2 (1 ::: 2 ::: 3 ::: Nil)

takeEx2 = take (-2) (1 ::: 2 ::: 3 ::: Nil)
  -}


































--------------------------------------------------------------------------------
-- Parameters may depend on previous parameters
--------------------------------------------------------------------------------

  {-
take' :: Int -> List a -> List a
take' 0 _   = Nil
take' _ Nil = error "Not enough items to take"
take' n (x ::: xs) =
  if n > 0
    then x ::: take' (n - 1) xs
    else error "Negative number of elements to take"

take'Ex :: List Int
take'Ex = take' 5 (1 ::: 2 ::: 3 ::: Nil)
  -}
































--------------------------------------------------------------------------------
-- Outputs can reveal information
--------------------------------------------------------------------------------

  {-
{-@ type ListGTE n = {xs:List a | len' xs >= n} @-}

{-@ take'' :: n:Nat -> ListGTE n -> List a @-}
take'' :: Int -> List a -> List a
take'' 0 _   = Nil
take'' _ Nil = error "Not enough items to take"
take'' n (x ::: xs) =
  if n > 0
    then x ::: take'' (n - 1) xs
    else error "Negative number of elements to take"

take''Ex  = take'' 1 (take'' 2 (1 ::: 2 ::: 3 ::: 4 ::: Nil))
  -}
































--------------------------------------------------------------------------------
-- Proofs require remination
--------------------------------------------------------------------------------

  {-
countSteps :: Int -> Int -> Int
countSteps stepSize n =
  if n == 0
    then 0
    else 1 + countSteps stepSize (n - stepSize)
  -}







































--------------------------------------------------------------------------------
-- Termination may need some help
--------------------------------------------------------------------------------

  {-
range :: Int -> Int -> List Int
range lo hi =
  if lo == hi
    then Nil
    else if lo < hi
      then lo ::: range (lo + 1) hi
      else error "The upper bound is lower than the lower bound."


rangeEx = range 1 4
  -}




































