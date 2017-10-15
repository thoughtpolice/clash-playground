{-# LANGUAGE GADTs          #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies   #-}

-- | A module for formal specifications and assertions of @'Signal'@ values.
module Assert
  ( -- * High level utilities
    verifyWithReset

    -- * Low-level formal property assertion
  , assertProperty
  , initialAssume

    -- * Utilities
  , getReset
  ) where
import           Prelude               hiding (pred)

import           Control.Applicative   (liftA2)
import           Data.Bits             ((.|.))
import           GHC.Stack             (HasCallStack)

import           Clash.Signal          (HasReset, hasReset, Signal)
import           Clash.Signal.Internal (Reset(..))

-- | Assert a SystemVerilog(-ish) property over a given @'Signal'@, returning
-- another @'Signal'@.
--
-- During both simulation and synthesis, @'assertProperty' b v@ is equivalent to
-- @v@. However, during verification (with @yosys-smtbmc@ or @symbiyosys@), this
-- asserts that the given @'Bool'@ @'Signal'@ always holds @'True'@.
assertProperty
  :: Signal dom Bool
  -- ^ The assertion to check. This is a stateful @'Signal'@ which is assumed to
  -- be some property to check over a given circuit.
  -> Signal dom a
  -- ^ Input @'Signal'@. This value is simply returned.
  -> Signal dom a
  -- ^ Output @'Signal'@. Identical to the input @'Signal'@.
assertProperty = \_ x -> x
{-# NOINLINE assertProperty #-}

-- | Assume a SystemVerilog(-ish) property for the initial clock cycle, over
-- a given @'Signal'@, returning another @'Signal'@.
--
-- During both simulation and synthesis, @'initialAssume' b v@ is equivalent to
-- @v@. However, during verification (with @yosys-smtbmc@ or @symbiyosys@), this
-- introduces the necessary assumption that the @'Bool'@ holds true on the
-- initial clock cycle; i.e. it tells that the given @'Bool'@ @'Signal'@ holds
-- @'True'@ the verification tool to only consider SMT traces where this case is
-- true.
initialAssume
  :: Signal dom Bool
  -- ^ Signal to assume true on the initial clock cycle.
  -> Signal dom a
  -- ^ Input @'Signal'@. This value is simply returned.
  -> Signal dom a
  -- ^ Output @'Signal'@. Identical to the input @'Signal'@.
initialAssume = \_ x -> x
{-# NOINLINE initialAssume #-}

-- | Convert an explicit @'Reset'@ into a @'Bool'@ @'Signal'@. This is often
-- used to assert/assume things about the reset during verification (e.g.
-- @'initialAssume'@ that the given @'Reset'@ is set on the first clock cycle,
-- to initialize registers).
getReset
  :: Reset dom sync
  -- ^ @'Reset'@ to convert.
  -> Signal dom Bool
  -- ^ @'Bool'@ @'Signal'@ for the given @'Reset'@.
getReset (Async rst) = rst
getReset (Sync rst)  = rst
{-# INLINEABLE getReset #-}

-- | Simple verification harness, for time-invariant properties over a
-- @'Signal'@, with a given @'Clock'@ and @'Reset'@ line. Given a clocked signal
-- @v@, the circuit @'verify' prop v@ asserts that @prop v@ should always be
-- @'True'@ for every clock cycle. @'verify'@ also abstracts over the
-- synchronous reset logic and properly assumes existence of the reset signal,
-- or that the property is upheld.
verifyWithReset
  :: ( HasCallStack
     , HasReset dom sync
     )
  => (a -> Bool)
  -- ^ A pure property @p@ that must hold @'True'@ over the given @'Signal'@
  -- value for every clock cycle.
  -> Signal dom a
  -- ^ The input @'Signal'@. The given property @p@ is checked against every
  -- value of the @'Signal'@ on every cycle.
  -> Signal dom a
  -- ^ The ouput @'Signal'@, identical to the input @'Signal'@ in all ways.
verifyWithReset pred sig
  = initialAssume rst    -- assume: rst is high on clk #0
  $ assertProperty check -- assert: either rst is high, or prop is ok
  $ sig
  where
    -- property: either the reset is high (clk #0), or the predicate for
    -- the signal passes if it is not (on any clock cycle).
    check = rst .||. (pred <$> sig)

    -- reset line, converted to a Bool, used for our verification assumptions.
    -- 'hasReset' ties the explicit reset argument to our implicit reset line.
    rst = getReset hasReset

    -- utility: lift the pure, bitwise-or operator over a stateful Signal
    (.||.) = liftA2 (.|.)
{-# INLINE verifyWithReset #-}
