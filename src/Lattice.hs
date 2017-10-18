{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- |
-- Module      : Clash.Lattice.ICE40
-- Copyright   : (c) Austin Seipp 2017
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (Clash language)
--
-- This module provides usable Clash interfaces to the Lattice Semiconductor
-- (formerly /Silicon Blue/) iCE40 Technology Library.
--
-- The primary use of this module is for @'Clock'@ sources that are wired
-- to the on-board iCE40 Phase-Locked Loops (PLLs), but interfaces to other
-- useful low-level primitives are provided, as necessary.
--
-- This module is intended to be simulation-compatible: you should be able to
-- both synthesize Clash code to working RTL, as well as use it for valid
-- simulation when running Haskell simulations. Where this isn't applicable, it
-- is noted.
--
-- Simulation of /synthesis results/ (i.e. output of the Clash) compiler and
-- simulation of /post-fitting results/ (i.e. after full synthesis, followed by
-- place-and-route) requires a capable synthesis and place-and-route tool, with
-- simulation-compatible iCE40 technology library primitives.
--
-- Worthy references you should read in order to use this module correctly:
--
--   * <http://www.latticesemi.com/~/media/LatticeSemi/Documents/DataSheets/iCE/iCE40LPHXFamilyDataSheet.pdf Lattice iCE40 LP\/HX Family Datasheet>.
--     This is the primary architectural reference for the iCE40 series FPGA
--     family, containing information on the clock network, routing, and design
--     of the PLBs (Programmable Logic Blocks) available on-board. The
--     \"Architectural Overview\" should be your primary reference for the
--     high-level layout of the chip.
--
--   * <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf Lattice iCE Technology Library>.
--     This is the primary reference for all of the technology library
--     primitives available on-chip, and available in this library. Where
--     applicable, any deviations or necessary information is highlighted here.
--     (In the event this module and the tech library reference are in conflict,
--     it is almost certainly a bug in this library.)
--
--   * <https://www.latticesemi.com/~/media/LatticeSemi/Documents/ApplicationNotes/IK/iCE40ProgrammingandConfiguration.pdf?document_id=46502 iCE40 Programming and Configuration>.
--     This document describes the boot mechanisms of the iCE40. Of particular
--     relevance is the documentation concerning cold and warm boot modes.
--
--   * <https://github.com/cliffordwolf/yosys/blob/0ccfb88728c5bdf167b1d672034ca5cf360dedb1/techlibs/ice40/cells_sim.v iCE40 Tech Library Simulation module>.
--     This code, part of the <http://www.clifford.at/yosys/ Yosys Open Synthesis Suite>,
--     contains simulation-oriented Verilog code defining most of the iCE40 cell
--     primitives. This serves as a useful reference to understand the
--     behavioral operation of many of these primitives.
--
--   * <http://www.clifford.at/icestorm/ Project Icestorm> by Clifford Wolf.
--     This page contains fully reverse-engineered tools and documentation for
--     the iCE40 series FPGAs including bitstream, RAM, IO, and LOGIC tile
--     layout. It should serve as the /ultimate/ reference for any questions
--     about the chip or implementation of the synthesis tools.
--
module Lattice
  ( -- ** PLL Primitives
    -- $plls
    sbPll40

    -- ** Global Buffer Primitives
    -- $globalbufs
  , sbGlobalBuffer

    -- ** Device Configuration Primitives
    -- $deviceconfig
  , WarmBootImage(..)
  , sbWarmBoot

    -- ** Combinational Logic Primitives
    -- $combinational

    -- *** @'Signal'@ primitives
  , sbLut4, sbCarry, sbLut4Carry
    -- *** Pure primitives
  , sbLut4#, sbCarry#

    -- ** Block RAM Primitives
    -- $blockram
  , sbRam4K#

    -- ** Register Primitives
    -- $regs
  ) where
import           Prelude
import           Data.Bits
import           Unsafe.Coerce               (unsafeCoerce)

import           GHC.TypeLits

import           Clash.Class.BitPack         (BitPack(..))
import           Clash.NamedTypes            ((:::))
import           Clash.XException            (errorX)

import           Clash.Prelude.BitIndex      ((!), slice)
import           Clash.Promoted.Nat          (SNat(..), snatToNum)
import           Clash.Promoted.Nat.Literals
import           Clash.Promoted.Symbol       (SSymbol(..))
import           Clash.Signal.Internal
import           Clash.Sized.BitVector       (Bit, BitVector, high)
import           Clash.Sized.Index           (Index)
import           Clash.Sized.Unsigned        (Unsigned)

--------------------------------------------------------------------------------
-- Phase Locked Loops (PLLs)

{- $plls

These functions provide Phase-locked loop (PLL) primitives for iCE40 boards,
allowing users to create their own clock signals of a specific frequency from
the on-board system clock.

The primary primitive for this is the @'sbPll40'@ function, which uses the
@SB_PLL40_CORE@ technology library primitive in order to create a PLL from a
reference clock input.

-}

-- | A @'Clock'@ source that corresponds to the @SB_PLL40_CORE@ component of the
-- iCE40 technology library.
--
-- The @feedbackPath@, @divr@, @divf@, @divq@, and @filter@ type parameters
-- correspond to the @FEEDBACK_PATH@, @DIVR@, @DIVF@, @DIVQ@ and @FILTER_RANGE@
-- parameters to the @SB_PLL40_CORE@ module, respectively. All of these
-- parameters __MUST__ be constant values at compile-time.
--
-- The intended means of using this primitive is to generate the necessary
-- parameters using the iCECube2 PLL Configuration tool, or alternatively using
-- the @icepll@ tool, available in Project IceStorm. For example, using @icepll@
-- to generate a 60mhz PLL clock from the on-board 12mhz clock of the iCE40 can
-- be done by first generating the parameters:
--
-- @
-- $ icepll -i 12 -o 60
--
-- F_PLLIN:    12.000 MHz (given)
-- F_PLLOUT:   60.000 MHz (requested)
-- F_PLLOUT:   60.000 MHz (achieved)
--
-- FEEDBACK: SIMPLE
-- F_PFD:   12.000 MHz
-- F_VCO:  960.000 MHz
--
-- DIVR:  0 (4'b0000)
-- DIVF: 79 (7'b1001111)
-- DIVQ:  4 (3'b100)
-- FILTER_RANGE: 1 (3'b001)
-- @
--
-- These parameters can be applied to generate an appropriate PLL and tie it to
-- a @'Clock'@ and @'Reset'@ line, like so:
--
-- @
-- let (pllOut, pllStable) = sbPll40 (SSymbol @"SIMPLE") d0 d79 d4 d1 clk rst
--      rstSync = resetSynchronizer pllOut (unsafeToAsyncReset pllStable)
-- in withClockReset pllOut rstSync ...
-- @
sbPll40
  :: "FEEDBACK_PATH" ::: SSymbol feedbackPath
  -- ^ @FEEDBACK_PATH@ parameter, which selects the feedback path to the PLL
  -- component. Either @\"SIMPLE\"@ or @\"NON_SIMPLE\"@. __MUST__ be a
  -- compile-time constant.

  -> "DIVR"          ::: SNat divr
  -- ^ @DIVR@, the @REFERENCECLK@ divider. __MUST__ be a compile-time
  -- constant.

  -> "DIVF"          ::: SNat divf
  -- ^ @DIVF@, the Feedback divider. __MUST__ be a compile-time constant.

  -> "DIVQ"          ::: SNat divq
  -- ^ @DIVQ@, the VCO divider. __MUST__ be a compile-time constant.

  -> "FILTER_RANGE"  ::: SNat filter
  -- ^ @FILTER_RANGE@, the PLL filter range. __MUST__ be a compile-time
  -- constant.

  -> "REFERENCECLK"  ::: Clock inp 'Source
  -- ^ Input PLL clock source for @SB_PLL40_CORE@. Typically, this is the
  -- on-board 12mhz oscillator (or similar).

  -> "RESETB"        ::: Reset inp 'Asynchronous
  -- ^ Reset port that asynchronously resets the PLL. __NOTE: This port is__
  -- __active low input!__

  -> ( "PLLOUTCORE" ::: Clock out 'Source
     , "RST"        ::: Signal out Bool
     )
sbPll40 _ _ _ _ _ clk (Async rst) = ( unsafeCoerce (clockGate clk rst)
                                    , unsafeCoerce rst
                                    )
{-# NOINLINE sbPll40 #-}

--------------------------------------------------------------------------------
-- Global Buffers

{- $globalbufs

These functions offer primitives for interfacing @'Signal'@ values with the
on-board, low-skew, high-fanout, global routing network.

-}

-- | Globally buffer an input @'Signal'@ through the high-fanout global routing
-- network.
sbGlobalBuffer
  :: Signal dom Bool
  -- ^ Input @'Signal'@ to buffer through the global network.
  -> Signal dom Bool
  -- ^ Resulting buffered @'Signal'@.
sbGlobalBuffer = \x -> x
{-# NOINLINE sbGlobalBuffer #-}

--------------------------------------------------------------------------------
-- Device Configuration

{- $deviceconfig

The iCE40 family contains a single on-board device configuration feature: \"warm
boot\", allowing a programmed image to boot into another programmed image that
has been baked into the bitstream.

Warm boot-able bitstreams can be created using the @icemulti@ tool available in
Project IceStorm. This tool can format up-to 4 images together in a single
bitstream, and lets you select the default boot image to load. Once this is
done, you can boot a new image at runtime via the @ADDR@ parameter of the
@'sbWarmBoot'@ primitive. The boot image is selected using a two-bit /vector/
/address/ value, from 0 to 3, specifying which image to boot.

An example is available in
<https://github.com/cliffordwolf/icestorm/tree/master/examples/icemulti the
icestorm source code. The very brief summary is to package your @.bin@ files
after using @icepack@ as follows:

@
$ icemulti -v -A16 -p0 -o bitstream.bin 0.bin 1.bin 2.bin 3.bin
@

This packages @0.bin@ at vector address 0, and likewise for @1.bin@, @2.bin@ and
@3.bin@. The file @bitstream.bin@ is the final bitstream containing the other 4
images, which can be booted using boot addresses @0-3@ via the @'sbWarmBoot'@
primitive.

-}

-- | The warm boot image to boot-up.
--
-- This type is purely a useful mnemonic device: it is an instance of
-- @'BitPack'@ with a size of two, meaning it can be used as an argument to
-- @'sbWarmBoot'@ to select the boot image. This gives a clear indication to
-- your users which image you want to select, and allows exhaustive case
-- analysis.
data WarmBootImage
  = SbWarmImage0 -- ^ Image #0
  | SbWarmImage1 -- ^ Image #1
  | SbWarmImage2 -- ^ Image #2
  | SbWarmImage3 -- ^ Image #3
  deriving (Eq, Show, Enum, Bounded)

instance BitPack WarmBootImage where
  type BitSize WarmBootImage = 2

  pack :: WarmBootImage -> BitVector 2
  pack x = fromIntegral (fromEnum x)
  {-# INLINE pack #-}

  unpack :: BitVector 2 -> WarmBootImage
  unpack x = toEnum (fromIntegral x)
  {-# INLINE unpack #-}

-- | Warm booting primitive -- allows the user to load a new bitstream
-- configuration during regular operation. Users can pack up-to 4 images into a
-- single iCE40 @.bin@ bitstream file.
--
-- The type of the input @addr@ line is parameterized over any @'BitPack'@-able
-- type of size two. This two bit value selects which of the 4 images to boot.
-- Overloading on the type is convenient because it lets you select the boot
-- image from a variety of types, including user defined ones -- such as the
-- included @'WarmBootImage'@ type, which gives a clear indication of which
-- image you intend to select.
--
-- /Note: Warm Boot mode is different from Cold Boot, executed during initial/
-- /device boot-up sequence./
sbWarmBoot
  :: ( BitPack a, BitSize a ~ 2
     ) =>
     "BOOT" ::: Signal dom Bit
  -- ^ Triggering @'Signal'@, used to initiate boot transfer. This @'Signal'@ is
  -- purely level sensitive.

  -> "ADDR" ::: Signal dom a
  -- ^ Two bit address, specifying which of the 4 pre-defined configuration
  -- images present in the iCE40 bitstream to transfer control to. This type
  -- may be any @'BitPack'@-able type of size two. A useful type that is
  -- easy to read for this purpose is @'WarmBootImage'@.

  -> Signal dom ()
  -- ^ Nullary @'Signal'@ result.
sbWarmBoot boot addr = sbWarmBoot# boot (fmap (!. 1) v) (fmap (!. 0) v)
  where v = fmap pack addr
{-# INLINEABLE sbWarmBoot #-}

-- | Direct mapping to @SB_WARMBOOT@ for @'sbWarmBoot'@. Defined separately, so
-- arguments are easier to pass onto the primitive -- by using multiple
-- arguments (not a tuple) for the inputs.
sbWarmBoot#
  :: "BOOT" ::: Signal dom Bit
  -> "S1"   ::: Signal dom Bit
  -> "S0"   ::: Signal dom Bit
  -> Signal dom ()
sbWarmBoot# = \_ _ _ -> pure ()
{-# NOINLINE sbWarmBoot# #-}

--------------------------------------------------------------------------------
-- Combinational Logic

{- $combinational

\"Combinational Logic\" primitives. These encapsulate the \"pure functional\"
logic available on the board: the 4-LUT and the carry chain, each being a pure
function of their inputs.

iCE40 devices feature a global carry chain connected between adjacent PLBs in
the routing network. Every PLB contains a 4-input LUT as well as dedicated carry
logic, with 2 of the 4 LUT inputs and the 2 carry inputs connected directly.
Carry logic outputs directly wire to adjacent carry logic inputs to route
carries globally.

Note that while these circuits are in theory pure, they are exposed with
@'Signal'@ interfaces here, since most users will want to propagate @'Signal'@
values both into and out of them. The pure variants are also exposed for
testing, but they also map onto the technology library primitives during
synthesis.

-}

--
-- Sequential logic exports
--

-- | Combination @SB_LUT4@ and @SB_CARRY@ cells, tying the @I1@ and @I2@ ports
-- of an @SB_LUT4@ to the @I0@ and @I1@ inputs of a @SB_CARRY@, respectively.
-- This reflects the underlying dedicated wiring of LUTs to Carry cells inside
-- the iCE40 PLB.
sbLut4Carry
  :: forall n dom.
     ( n <= (2^16)-1
     )
  => "LUT_INIT" ::: SNat n
  -> "IO"       ::: Signal dom Bit
  -> "I1"       ::: Signal dom Bit
  -> "I2"       ::: Signal dom Bit
  -> "I3"       ::: Signal dom Bit
  -> "CI"       ::: Signal dom Bit
  -> ( "O"  ::: Signal dom Bit
     , "CO" ::: Signal dom Bit
     )
sbLut4Carry s@SNat i0 i1 i2 i3 ci = (o, co)
  where
    o  = sbLut4 s i0 i1 i2 i3 -- LUT4 out
    co = sbCarry     i1 i2 ci -- Carry out
{-# INLINEABLE sbLut4Carry #-}

-- | Mapping to an @SB_LUT4@ primitive: a simple ROM 4-input look-up function.
-- This function is effectively @'sbLut4#'@, but lifted to @'Signal'@ values.
sbLut4
  :: forall n dom.
     ( n <= (2^16)-1
     )
  => "LUT_INIT" ::: SNat n
  -> "IO"       ::: Signal dom Bit
  -> "I1"       ::: Signal dom Bit
  -> "I2"       ::: Signal dom Bit
  -> "I3"       ::: Signal dom Bit
  -> "O"        ::: Signal dom Bit
sbLut4 s@SNat i0 i1 i2 i3 =
  sbLut4# s <$> i0 <*> i1 <*> i2 <*> i3
{-# INLINEABLE sbLut4 #-}

-- | Mapping to an @SB_CARRY@ primitive: fast carry logic with global chain.
-- This function is effectively @'sbCarry4#'@, but lifted to @'Signal'@ values.
sbCarry
  :: "I0" ::: Signal dom Bit
  -> "I1" ::: Signal dom Bit
  -> "CI" ::: Signal dom Bit
  -> "C0" ::: Signal dom Bit
sbCarry i0 i1 ci = sbCarry# <$> i0 <*> i1 <*> ci
{-# INLINEABLE sbCarry #-}

--
-- Pure simulation logic
--

-- | @SB_LUT4@ primitive, exposed as a pure function. Note that most users will
-- want to use @'sbLut4'@ instead to tie @'Signal'@ values together.
--
-- This function works for both simulation and synthesis. During synthesis it
-- maps to @SB_LUT4@ directly.
sbLut4#
  :: forall n.
     ( KnownNat n
     , n <= (2^16)-1
     )
  => "LUT_INIT" ::: SNat n
  -> "IO" ::: Bit
  -> "I1" ::: Bit
  -> "I2" ::: Bit
  -> "I3" ::: Bit
  -> "O"  ::: Bit
sbLut4# s@SNat i0 i1 i2 i3 = o
  where
    v = snatToNum s :: BitVector 16
    isHigh b x y = if (b == high) then x else y

    s3 = isHigh i3 (slice d15 d8 v) (slice d7 d0 v)  :: BitVector 8
    s2 = isHigh i2 (slice d7 d4 s3) (slice d3 d0 s3) :: BitVector 4
    s1 = isHigh i1 (slice d3 d2 s2) (slice d1 d0 s2) :: BitVector 2
    o  = isHigh i0 (s1 !. 1)        (s1 !. 0)        :: Bit
{-# NOINLINE sbLut4# #-}

-- | @SB_CARRY@ primitive, exposed as a pure function. Note that most users will
-- want to use @'sbCarry'@ instead to tie @'Signal'@ values together.
--
-- This function works for both simulation and synthesis. During synthesis
-- it maps to the @SB_CARRY@ primitive directly.
sbCarry#
  :: "I0" ::: Bit
  -> "I1" ::: Bit
  -> "CI" ::: Bit
  -> "C0" ::: Bit
sbCarry# i0 i1 ci = (i0 .&. i1) .|. ((i0 .|. i1) .&. ci)
{-# NOINLINE sbCarry# #-}

--------------------------------------------------------------------------------
-- Block RAM

{- $blockram

Block RAM primitives. iCE40 FPGAs feature several 4K BlockRAM entities (e.g. 32
on the HX8K and 16 on the HX1K) that can be programmed fully by the user. The
primary Clash interface for this primitive is @SB_RAM40_4K@, encapsulated by
@'sbRam4K'@.

Note: while the iCE40 library itself provides several variants of the
@SB_RAM40_4K@ primitive for @posedge@\/@negedge@ clocked read\/write ports, this
library only offers the default primitive which offers positive clock edge read
and write ports.

-}

-- | Direct mapping to an @SB_RAM40_4K@ primitive, the basic physical RAM
-- building block which can be configured to different depth and data ports.
-- This BRAM has a size of 4Kbits with separate read/write ports, each with an
-- independent control signals.
--
-- Block RAM contents may optionally be pre-loaded during ICE40 device
-- configuration.
sbRam4K#
  :: "RCLK"  ::: Clock rdom 'Source
  -> "WCLK"  ::: Clock wdom 'Source

  -> "RCLKE" ::: Signal rdom Bool
  -> "RE"    ::: Signal rdom Bool
  -> "RADDR" ::: Signal rdom (Unsigned 10)

  -> "WCLKE" ::: Signal wdom Bool
  -> "WE"    ::: Signal wdom Bool
  -> "WADDR" ::: Signal wdom (Unsigned 10)

  -> "MASK"  ::: Signal wdom (Unsigned 16)
  -> "WDATA" ::: Signal wdom (BitVector 16)

  -> "RDATA" ::: Signal rdom (BitVector 16)
sbRam4K# = errorX "TODO FIXME: NIH"
{-# NOINLINE sbRam4K# #-}

--------------------------------------------------------------------------------
-- Registers

{- $regs

TODO FIXME: Not implemented here!

-}

--------------------------------------------------------------------------------
-- Extra utilities

-- | Specialized @'BitVector'@ index operator with a specialized @'Index'@ type
-- to avoid \"default inferred type\" warnings.
(!.) :: BitVector 2 -> Index 2 -> Bit
(!.) = (!) -- specialize index type
{-# INLINE (!.) #-}
