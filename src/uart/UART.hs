{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module UART
  ( top
  ) where

--------------------------------------------------------------------------------

import           Clash.Prelude
import           Clash.Prelude.Safe    (riseEvery)
import           Clash.Promoted.Symbol (SSymbol(..))

import           GHC.TypeLits.Extra    (Div)

import           Data.Bool             (bool)

import           Lattice
import           TopGen
import           Utils

--------------------------------------------------------------------------------
-- Utilities

-- | Extract the period (@'Nat'@) from a @'Clock'@ @'Domain'@. This makes
-- certain types a bit nicer to write.
type family Period (a :: Domain) :: Nat where
  Period ('Dom nam period) = period

-- | Provide numeric evidence of a clock domain's period. The @dom@ is
-- expected to be applied using @TypeApplications@
clockPeriod :: forall dom. KnownNat (Period dom) => SNat (Period dom)
clockPeriod = SNat

--------------------------------------------------------------------------------

data TxState
  = TxIdle
  | TxStart   (Unsigned 8)
  | TxSending (Unsigned 8) (Index 8)
  | TxDone
  deriving (Eq, Show, Ord)

-- | UART Transmission (TX) state machine logic. This controls the transition
-- logic for the transmission @'Signal'@s.
uartTxT
  :: TxState
  -> Maybe (Unsigned 8)
  -> (TxState, (Bit, Bool))

-- Done state: return to idle state, drive the line high, and note that we are
-- finished.
uartTxT TxDone            _        = (TxIdle, (high, True))

-- Idle state, nothing to send: remain idle, drive high, unfinished.
uartTxT TxIdle            Nothing  = (TxIdle       , (high , False))

-- Idle state, byte submitted: start transmitting, transmit 0 bit to signal
-- change of state, continue unfinished.
uartTxT TxIdle            (Just b) = (TxStart b    , (low  , False))

-- Start state: move to sending state and send the first bit, continue
-- unfinished.
uartTxT (TxStart b)       _        = (TxSending b 0, (lsb b, False))

-- Sending state: send bits down the line, and determine whether to continue
-- sending, or transition to the finished state.
uartTxT (TxSending b off) _
  -- if we've sent all the bits, then we're done.
  | off == 7
  = (TxDone, (high, False))

  -- otherwise, transmit the lsb of the current byte, increment the offset,
  -- and keep sending
  | otherwise
  = (TxSending b' off', (lsb b', False))
  where b'   = b `shiftR` 1 -- shift off lower bit
        off' = off + 1      -- new offset
{-# INLINEABLE uartTxT #-}

-- | The Transmit (TX) component of a RS232-compatible UART core.
uartTx
  :: HasClockReset dom gate sync
  -- ^ Requires a @'Clock'@ and @'Reset'@ line.

  => Signal dom Bool
  -- ^ Enable line; when driven @'high'@, the line line is active and output
  -- @'Signal'@ data is being generated.

  -> Signal dom (Maybe (Unsigned 8))
  -- ^ Optional byte to transmit down the line.

  -> Signal dom (Bit, Bool)
  -- ^ Output: the resulting @'Bit'@ to transfer down the TX line to the
  -- receiver, and a @'Bool'@ indicating whether or not the current bit is
  -- finished sending. If the @'Bool'@ is @'True'@, then the TX line is idle and
  -- waiting for more data.

uartTx en x = bundle (register high out, finished .&&. en)
  where (out, finished) = unbundle (mealyEn uartTxT TxIdle en x)
{-# INLINEABLE uartTx #-}

--------------------------------------------------------------------------------

-- | A UART TX component that automatically derives a proper enable line
-- given a baud rate and a clock domain with a correct period.
uartTxF
  :: forall speed dom delay nam period gate sync.
     ( dom ~ 'Dom nam period
     , HasClockReset dom gate sync

     , KnownNat speed, KnownNat delay, KnownNat period
     , delay ~ Div period speed
     )
  => Signal dom (Maybe (Unsigned 8))
  -> Signal dom (Bit, Bool)
uartTxF = uartTx en where en = riseEvery (SNat @delay)

--------------------------------------------------------------------------------
-- Top level exports

-- | The on-board system clock. ICE40 boards feature a 12mhz oscillator.
type ClkSys = 'Dom "system" 12000000 -- 12mhz onboard oscillator

-- | The \"system\" clock, provided by the onboard ICE40 PLL component, clocked
-- at 60mhz.
type Clk60  = 'Dom "main"   60000000 -- 60mhz pll-generated clock

-- | Top-level circuit. This encapsulates all of the logic for the exported
-- core that will be put directly onto the board.
circuit
  :: HasClockReset Clk60 gate sync
  => ( "UART_TX" ::: Signal Clk60 Bit
     , "LED0"    ::: Signal Clk60 Bit
     )
circuit =
  let
    flip1hz = riseEvery (clockPeriod @Clk60)

    ascii :: Signal Clk60 (Unsigned 8)
    ascii = regEn 0x30 flip1hz $ ascii <&> \x ->
      bool (x+1) 0x30 (x == 0x39)

    blink = regEn low flip1hz (complement <$> blink)
    (tx, _) = unbundle $ uartTxF @9600 @Clk60 (fmap Just ascii)

  in (tx, blink)

-- | Top entity to be exported to RTL. This essentially just wires the
-- @'circuit'@ up to the on-board PLL component to provide a real clock
-- source for actual synthesis.
top
  :: "CLK"       ::: Clock ClkSys 'Source
  -> ( "UART_TX" ::: Signal Clk60 Bit
     , "LED0"    ::: Signal Clk60 Bit
     )
top clk =
  let (clk, rst) = ice40Top clk (pure True) -- fake reset signal
  in withClockReset clk rst circuit
$(makeTopEntityWithName 'top "uart") -- auto generate topentity
