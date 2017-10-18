module Utils
  ( mealyEn
  ) where

import           Clash.Prelude

-- | Mealy machine with an enable line.
mealyEn
  :: ( HasClockReset dom gate sync
     )
  => (s -> i -> (s, o))
  -> s
  -> Signal dom Bool
  -> Signal dom i
  -> Signal dom o
mealyEn f iS en = \i -> let s = regEn iS en s'
                            (s',o) = unbundle (f <$> s <*> i)
                        in  o
{-# INLINEABLE mealyEn #-}
