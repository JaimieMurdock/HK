HaskellKanren
================
HaskellKanren is an implementation of the Kanren language described in Daniel P.
Friedman, William Byrd, and Oleg Kiselyov's *The Reasoned Schemer* (MIT Press,
2005). This project was created as the final project for Amr Sabry's CSCI-B490
Advanced Functional Programming course by Carlo Angiuli, Adam Foltzer, and
Jaimie Murdock.

Files
--------
src/Language/HK.hs
    Defines the HaskellKanren Language
src/Language/HK/Prelude.hs   
    Defines an environment for using HaskellKanren
test/HKTests.hs      
    Defines a suite of test cases
Examples.hs
    Collection of examples using HKPreludeo

Dependencies
-----------------
logict
    http://hackage.haskell.org/package/logict 

Documentation
-----------------
Use ``cabal haddock`` to build current documentation.

About
-------
HaskellKanren is strongly-typed and uses a pure functional syntax. It relies on
Haskell's built-in type-checking to reject mistyped programs. For instance, we would 
like ``run $ \q -> (quote 5) === (quote "foo")`` to be statically rejected,
since we know that unification can never succeed.  However, this requires that
the substitutions at the core of our system maintain mappings from logic
variables to distinctly typed values, in other words, a heterogenous data
structure. Inspired by Claessen & Ljunglöf, we attempted to use references in
Haskell's ST monad as a way to store heterogenously-typed values in a way that
would allow unification.

The project back-end was designed to use monadic computations to control
backtracking with fair disjunction. The most difficult issue with this approach
is how to save and reset the state between interleaving branches of computation.
Claessen & Ljunglöf show a method of lifting writeSTRef into the basic list
monad; in this method, a variable's value is read and saved before being
overwritten, so that the old value may be restored after the computation
continues. With the more complex LogicT of Kiselyov et al., this level of
control isn't sufficient -- we have no guarantee that control will return to the
site of the reference write before switching to another branch of computation.
This allows the effects of unification to be seen in branches which should have
no knowledge of those effects, making the logic system malfunction.

Our final implementation uses Claessen & Ljunglöf's original State type, a pair
of the current substitution and an infinite stream of uninstantiated variables.
This ensures that each new exist has access to the next uninstantiated variable.
Substitutions, in turn, are association lists of variable bindings; unification
requires walking each term through the substitution, and possibly adding a new
binding.

URLs
------------
GitHub      
    http://github.com/JaimieMurdock/HK
Hackage 
    http://hackage.haskell.org/package/hk

