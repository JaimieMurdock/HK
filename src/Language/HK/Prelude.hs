module Language.HK.Prelude (
    module Language.HK
  , elemo
  , appendo
  ) where

import Language.HK hiding (
    and2
  , or2
  , unify
  , emptyS
  , extendS
  , walk
  , reify
  , Subst
  , State
  )

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | @elemo@ relates a value to a list if and only if that value is an
-- element of that list.
elemo x xs = or' [first x xs,
                  exist (\d -> and' [second d xs, elemo x d])]

-- | @appendo@ relates two lists to the result of appending those
-- lists.
appendo :: Val  -- ^ starting list
        -> Val  -- ^ value to append
        -> Val  -- ^ ending list
        -> Pred -- ^ goal
appendo ls s out = or' [and' [Nil === ls, s === out],
                        (existN 3 (\[a,d,res] ->
                                  and' [pair a d ls,
                                        appendo d s res,
                                        pair a res out]))]
