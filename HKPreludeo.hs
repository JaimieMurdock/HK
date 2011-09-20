module HKPreludeo(module HK, elemo, appendo) where

import HK hiding (and2, or2, unify, emptyS, extendS, walk, reify, Subst, State)

-------------------------------------------------------------------------------
-- Preludeo
-------------------------------------------------------------------------------

-- | @elemo@ forms one half of the HKPrelude, simply giving a predicate for
-- testing list membership.
elemo x xs = or' [first x xs,
                  exist (\d -> and' [second d xs, elemo x d])]

-- | @appendo@ is the other half of the HKPrelude, allowing for list construction 
-- in a logic program.
appendo :: Val  -- ^ starting list
        -> Val  -- ^ value to append
        -> Val  -- ^ ending list
        -> Pred -- ^ goal
appendo ls s out = or' [and' [Nil === ls, s === out],
                        (existN 3 (\[a,d,res] ->
                                  and' [pair a d ls,
                                        appendo d s res,
                                        pair a res out]))]
