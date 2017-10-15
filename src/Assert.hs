{-# LANGUAGE GADTs          #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies   #-}

-- | A module for formal specifications and assertions of @'Signal'@ values.
module Assert
  ( -- * High level utilities
    verifyWithReset

    -- ** Implication
  , (|->)             -- :: Property dom -> Property dom -> Property dom

    -- * Low-level formal property assertion
    -- ** @initial@ clock cycle assertions
    , initialAssume   -- :: Property dom -> Signal dom a -> Signal dom a
    , initialAssert   -- :: Property dom -> Signal dom a -> Signal dom a

    -- ** Concurrent assertions
    , assumeProperty  -- :: Property dom -> Signal dom a -> Signal dom a
    , assertProperty  -- :: Property dom -> Signal dom a -> Signal dom a

    -- * Utilities
    , getReset        -- :: Reset dom sync -> Signal dom Bool
  ) where
import           Prelude               hiding (pred)

import           Control.Applicative   (liftA2)
import           Data.Bits             ((.|.))
import           GHC.Stack             (HasCallStack)

import           Clash.Signal          (HasReset, hasReset, mux, Signal)
import           Clash.Signal.Internal (Reset(..))

--------------------------------------------------------------------------------
-- Types

-- | A @'Property' dom@ is a @'Signal'@ that represents a property over
-- the given clock domain.
type Property dom = Signal dom Bool

--------------------------------------------------------------------------------
-- Low level utilities

-- Initial (clock cycle 0) assertions

-- | @assume@ an @'Property'@ holds on the initial clock cycle of the circuit.
-- This is generally used to set up base assumptions (e.g. about @'Reset'@
-- lines) for the verification tool during cycle 0.
initialAssume
  :: Property dom
  -> Signal dom a
  -> Signal dom a
initialAssume = \_ x -> x
{-# NOINLINE initialAssume #-}

-- | @assert@ that a @'Property'@ holds on the initial clock cycle of the
-- circuit. This is used to establish basic properties that should be true
-- during the initial cycle and/or reset.
initialAssert
  :: Property dom
  -> Signal dom a
  -> Signal dom a
initialAssert = \_ x -> x
{-# NOINLINE initialAssert #-}

-- Concurrent assertions

-- | \"Concurrent assertion\", i.e. @assert@ a statement continuously. This
-- statement in Verilog:
--
-- @
-- assert property (...);
-- @
--
-- is equivalent to:
--
-- @
-- always @* asssert(...);
-- @
--
-- This effectively states the property must hold forever on all edges of the
-- circuit.
--
-- The statement @'assertProperty prop v'@ otherwise returns @v@ for all inputs
-- during Haskell simulation.
assertProperty
  :: Property dom
  -> Signal dom a
  -> Signal dom a
assertProperty = \_ x -> x
{-# NOINLINE assertProperty #-}

-- | \"Concurrent assumption\", i.e. @assume@ a statement continuously. This
-- statement in Verilog:
--
-- @
-- assume property (...);
-- @
--
-- is equivalent to:
--
-- @
-- always @* assume(...);
-- @
--
-- This effectively states the property must hold forever on all edges of the
-- circuit.
--
-- The statement @'assumeProperty prop v'@ otherwise returns @v@ for all inputs
-- during Haskell simulation.
assumeProperty
  :: Property dom
  -> Signal dom a
  -> Signal dom a
assumeProperty = \_ x -> x
{-# NOINLINE assumeProperty #-}

--------------------------------------------------------------------------------
-- Implication

-- | Logical implication for @'Signal'@ properties.
--
-- __NB__: This function can be synthesized to the SystemVerilog @|->@ operator,
-- but this function __IS NOT__ currently supported by Yosys! It is only here
-- for completeness.
(|->)
  :: Property dom -- ^ Property @P@
  -> Property dom -- ^ Property @Q@
  -> Property dom -- ^ Resulting property: @P |-> Q@
p |-> q = mux p q (pure True)
{-# NOINLINE (|->) #-}

infixr 0 |->

--------------------------------------------------------------------------------
-- High level utilities

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

--------------------------------------------------------------------------------
-- Extras

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

{--
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
--}
