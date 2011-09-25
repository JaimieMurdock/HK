{-# LANGUAGE FlexibleInstances #-}

-- |Module HK defines the HaskellKanren language, which is an
-- implementation of the specification in chapters 9 and 10 of The
-- Reasoned Schemer (Friedman, Byrd, and Kiselyov, 2005).
module Language.HK where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Logic hiding (fail)
import Data.List
import Data.Maybe

type Id = Integer

-- | The Val type allows us to pass along variables of any type within
-- the logic system, without respect to the internal type.
data Val = Var Id
         | String String
         | Integer Integer
         | Bool Bool
         | Pair (Val,Val)
         | Nil
         deriving (Show, Eq)

-- | Subst declares a substitution for the var Id with Val.
type Subst = [(Id,Val)]
-- | State stores a substitutions and a list of fresh variables.
type State = (Subst,[Val])

type Backtr a = Logic a
type Pred = State -> Backtr State

class LogicVal a where
  quote :: a -> Val

-- | Defines the empty subsitution.
emptyS :: Subst
emptyS = []

-- | Takes an Id and a Value and a current state, and extends the state.
extendS :: Val -> Val -> Subst -> Subst
extendS (Var x) v s = (x,v):s

-- | Walk moves through a current state (s) to discover the binding of the
-- Vartiable x.
walk :: Val -> Subst -> Val
walk var@(Var x) s =
  case lookup x s of
    Just e -> walk e s
    Nothing -> var
walk (Pair (x,y)) s = Pair (walk x s, walk y s)    
walk x s = x

-- | Unify is the core of the Kanren language. It checks if two variables are
-- bound, and if they are equal, returns the current state. Otherwise, the
-- substitution is extended such that the unbound variable is bound to the same
-- Var as the bound variable.
--
-- All results are returned using a Backtracking Monad, which is at present
-- using Monad.Control.Logic
unify :: Val -> Val -> Subst -> Backtr Subst
unify v w s = 
  case (walk v s, walk w s) of
    (v,w) | v == w -> return s
    (v@(Var x),w) -> return (extendS v w s)
    (v,w@(Var x)) -> return (extendS w v s)
    (Pair (x,y), Pair (x',y')) ->
      do s <- unify x x' s
         unify y y' s
    _ -> mzero

-- | The unification operator (===).
(===) :: Val -> Val -> Pred
v === w = \(subst,vs) -> do subst' <- unify v w subst
                            return (subst',vs)

-- | Monadic definitions of succeed and fail.
succeed, fail :: Pred
succeed s = return s
fail s = mzero

-- | Unificationn of `and`
and2 :: Pred -> Pred -> Pred
p `and2` q = \s-> p s >>- q

-- | Unificationn of `or` with fair disjunction from Logic monad. 
or2 :: Pred -> Pred -> Pred
p `or2` q = \s-> p s `interleave` q s


-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------

-- | and' provides an intuitive interface to the underlying and unification.
and' :: [Pred] -> Pred
and' = foldr and2 succeed

-- | or' provides an intuitive interface to the underlying disjunction
-- resolution.
or' :: [Pred] -> Pred
or' = foldr or2 fail

-- | exist allows for the introduction of new fresh variables.
exist :: (Val -> Pred) -> Pred
exist p = \(subst,v:vs) -> p v (subst,vs)

-- | existN is a simple interface built on top of exist, which allows for
-- multiple variables to be introduced.
existN :: Int               -- ^ Number of variables to introduce
       -> ([Val] -> Pred)   -- ^ List of fresh variables
       -> Pred              -- ^ Output goal
existN n p = \(subst,vs) -> let (use,fresh) = splitAt n vs
                              in p use (subst,fresh)

{--
existN' :: ([Val] -> Pred) -> Pred
existN' p = \(subst,vs) -> case vs of
                             []   -> p [] (subst,vs) 
                             x:xs -> let (use,fresh) = splitAt (length xs + 1) vs
                              in p use (subst,fresh)
--}
-- | Interface to run a logic program
run :: (Val -> Pred)  -- ^ A logic program
    -> [String]       -- ^ List of solutions formeted with @prettyVal@
run p = map (prettyVal . reify . (walk (Var 0)) . fst) $ observeAll (exist p zeroState)
  where zeroState = (emptyS,(map Var [0..]))

-- | Identical to @run@, except raw values are returned for use in
-- post-processing. For example, one would use runVals to utilize an adder which
-- returns binary numbers and convert them to ints in post-processing.
runVals :: (Val -> Pred) -- ^ A logic program 
        -> [Val]         -- ^ List of raw solutions
runVals p = map (reify . (walk (Var 0)) . fst) $ observeAll (exist p zeroState)
  where zeroState = (emptyS,(map Var [0..]))




instance LogicVal Val where
  quote = id

instance (LogicVal a) => LogicVal [a] where  
  quote = makeList

-- | Interface to create a single list Val from many LogicVal instances.
makeList :: (LogicVal a) => [a] -> Val
makeList [] = Nil
makeList (x:xs) = quote (x,makeList xs)

instance LogicVal [Char] where
  quote = String
  
instance LogicVal Integer where
  quote = Integer
  
instance LogicVal Bool where  
  quote = Bool

instance (LogicVal a, LogicVal b) => LogicVal (a,b) where
  quote (a,b) = Pair (quote a, quote b)

-- | helper functions for dealing with pairs
pair :: Val  -- ^ car
     -> Val  -- ^ cdr
     -> Val  -- ^ pair
     -> Pred -- ^ goal
pair x y p = p === Pair (x,y)

first :: Val  -- ^ car
      -> Val  -- ^ pair
      -> Pred -- ^ goal
first x p = exist (\y -> pair x y p)

second :: Val  -- ^ cdr
       -> Val  -- ^ pair
       -> Pred -- ^ goal
second y p = exist (\x -> pair x y p)



-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------
-- | Reification function which returns a reified name substitution in which
-- each variable in v has been associated with its reified name. 
reify :: Val -> Val
reify v = reify' (nub (flatten v)) v where
  reify' l v = case v of
    Var v -> Var (toInteger (fromJust (elemIndex v l)))
    Pair (x,y) -> Pair (reify' l x,reify' l y)
    _ -> v
  flatten x = case x of
    Var v -> [v]
    Pair (x,y) -> flatten x ++ flatten y
    _ -> []

-- | Pretty printer for Val data type to remove the data constructor from output.
prettyVal :: Val -> String
prettyVal (Var x) = "Var " ++ show x
prettyVal (Integer x) = show x                    
prettyVal (String s) = s
prettyVal (Bool b) = show b
prettyVal Nil = "()"
                
-- pairs are a bit tricky, since they have to start with an open paren,
-- but not give an open paren for each pair
prettyVal (Pair (x,Nil)) = "(" ++ prettyVal x ++ ")"
prettyVal (Pair (x,rest@(Pair (y,z)))) = "(" ++ prettyVal x ++ " " ++ prettyRest rest
  where prettyRest (Pair (x,Nil)) = prettyVal x ++ ")"
        prettyRest (Pair (x,rest'@(Pair (y,z)))) = 
          prettyVal x ++ " " ++ prettyRest rest'      
        -- "improper" list
        prettyRest (Pair (x,y)) = 
          prettyVal x ++ " . " ++ prettyVal y ++ ")"        
-- "improper" list
prettyVal (Pair (x,y)) = "(" ++ prettyVal x ++ " . " ++ prettyVal y ++ ")"  

