{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Simple counter module.
module Counter
  ( top
  ) where
import           GHC.Stack     (HasCallStack)
import           Clash.Prelude

import           TopGen        (makeTopEntityWithName)
import           Assert        (verifyWithReset)

-- | A counter circuit, that counts from 0 to 15 (inclusive) and then resets
-- to zero. This counter requires a clock and reset line to be attached.
counter
  :: ( HasCallStack                -- ^ Constraint: Contains a callstack
     , HasClockReset dom gate sync -- ^ Constraint: clock and reset lines
     )
  => Signal dom (Unsigned 6)       -- ^ Output: unsigned 6-bit number.
counter = verifyWithReset prop out
  where
    -- property: the output counter signal is always lower than the specified
    -- limit, which is 32, in this case.
    prop x = x < lim where lim = 32 :: Unsigned 6

    -- the counter: counts from 0 to 15, and resets back to 0. this is defined
    -- as a feedback loop using a register, and it simply maps a pure function
    -- onto the stateful component to apply the logic.
    out = register 0 (fmap f out)
    f x | x == 15   = 0
        | otherwise = x + 1

-- | Exported @'TopEntity'@. Ties @'counter'@ to an appropriate @'Clock'@ and
-- @'Reset'@.
top
  :: "clk" ::: Clock System 'Source
  -> "rst" ::: Reset System 'Synchronous
  -> "out" ::: Signal System (Unsigned 6)
top clk rst = withClockReset clk rst counter
$(makeTopEntityWithName 'top "counter") -- auto generate
