{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Multiboot
  ( app0
  , app1
  , app2
  , app3
  ) where

--------------------------------------------------------------------------------

import           Clash.Prelude

import           Lattice
import           TopGen
import           Utils

--------------------------------------------------------------------------------
-- Main circuit definition

type Log2Delay = 22

circuit
  :: HasClockReset dom gated sync
  => ( Signal dom Bit
     , Signal dom (BitVector 4)
     )
circuit = (state, counter2)
  where
    counter  = register (zero @Log2Delay) (counter   + 1)
    counter2 = register (zero @4)         (counter2  +  notA counter)
    state    = register (zero @1)         (state    .^. notA counter)

appMain
  :: Clock dom gated
  -> WarmBootImage
  -> Signal dom Bit
appMain clk next = out
  where
    rst = unsafeToAsyncReset (pure False)
    img = pure next
    reduce = fmap reduceAnd

    out = withClockReset clk rst
      (circuit & \(r, b) -> sbWarmBoot (reduce b) img r)

--------------------------------------------------------------------------------
-- Top-level modules, each exported into an image

app0
  :: "clk" ::: Clock System 'Source
  -> ( "LED1" ::: Signal System Bit
     , "LED2" ::: Signal System Bit
     , "LED3" ::: Signal System Bit
     , "LED4" ::: Signal System Bit
     , "LED5" ::: Signal System Bit
     )
app0 clk = (appMain clk SbWarmImage1, pure 0, pure 0, pure 0, pure 1)
$(makeTopEntity 'app0)

app1
  :: "clk" ::: Clock System 'Source
  -> ( "LED1" ::: Signal System Bit
     , "LED2" ::: Signal System Bit
     , "LED3" ::: Signal System Bit
     , "LED4" ::: Signal System Bit
     , "LED5" ::: Signal System Bit
     )
app1 clk = (pure 0, appMain clk SbWarmImage2, pure 0, pure 0, pure 1)
$(makeTopEntity 'app1)

app2
  :: "clk" ::: Clock System 'Source
  -> ( "LED1" ::: Signal System Bit
     , "LED2" ::: Signal System Bit
     , "LED3" ::: Signal System Bit
     , "LED4" ::: Signal System Bit
     , "LED5" ::: Signal System Bit
     )
app2 clk = (pure 0, pure 0, appMain clk SbWarmImage3, pure 0, pure 1)
$(makeTopEntity 'app2)

app3
  :: "clk" ::: Clock System 'Source
  -> ( "LED1" ::: Signal System Bit
     , "LED2" ::: Signal System Bit
     , "LED3" ::: Signal System Bit
     , "LED4" ::: Signal System Bit
     , "LED5" ::: Signal System Bit
     )
app3 clk = (pure 0, pure 0, pure 0, appMain clk SbWarmImage0, pure 1)
$(makeTopEntity 'app3)
