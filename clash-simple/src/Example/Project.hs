-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds #-}

module Example.Project where

import Clash.Prelude
import Data.Proxy

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem{vName="Dom30", vPeriod=hzToPeriod 30e6, vResetPolarity=ActiveLow}

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom30@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity
  :: Clock Dom30
  -> Reset Dom30
  -> Signal Dom30 Bit
  -> Signal Dom30 Bit
  -> Signal Dom30 Bit
  -> Signal Dom30 (Unsigned 14)
  -> Signal Dom30 (BitVector 16)
  -> Signal Dom30 Bit
  -> Signal Dom30 (BitVector 16)
  -> Signal Dom30 (Bit, Bit, Bit, Bit, Bit)
topEntity clk resetn btn1 btn2 btn3 addr wdata wen rdata =
  unpackVec5 . bitCoerce @(Unsigned 5) @(Vec 5 Bit) <$> exposeClockResetEnable accum clk resetn enableGen (bitToBool <$> btn1)

bitToUnsigned :: KnownNat n => Bit -> Unsigned n
bitToUnsigned = resize . bitCoerce

unpackVec5 :: Vec 5 a -> (a, a, a, a, a)
unpackVec5 (a :> b :> c :> d :> e :> Nil) = (a, b, c, d, e)

-- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN topEntity
  (Synthesize
    { t_name = "clash_top"
    , t_inputs = [ PortName "clk"
                 , PortName "resetn"

                 , PortName "btn1"
                 , PortName "btn2"
                 , PortName "btn3"

                 , PortName "ram_addr"
                 , PortName "ram_wdata"
                 , PortName "ram_wen"
                 , PortName "ram_rdata"
                 ]
    , t_output = PortProduct ""
                 [ PortName "led1"
                 , PortName "led2"
                 , PortName "led3"
                 , PortName "led4"
                 , PortName "led5"
                 ]
    }) #-}

-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

debouncer :: forall limit dom. (KnownNat limit, HiddenClockResetEnable dom) => Proxy limit -> Signal dom Bool -> Signal dom Bool
debouncer _ = mealy f (0, False)
  where
    f :: (Unsigned limit, Bool) -> Bool -> ((Unsigned limit, Bool), Bool)
    f (counter, out) inp =
      if out /= inp
         then if counter == maxBound
                 then ((0, inp), inp)
                 else ((counter + 1, out), out)
         else ((0, out), out)

-- | A simple accumulator that works on unsigned numbers of any size.
-- It has hidden clock, reset, and enable signals.
accum ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom Bool ->
  Signal dom (Unsigned n)
accum = mealy accumT (S 3 False) . debouncer (Proxy @10)
 where
  accumT S {lastButton, value} button =
    let value' = if not lastButton && button then value + 1 else value
    in
    (S value' button, value')

data S n = S
  { value :: Unsigned n
  , lastButton :: Bool
  }
  deriving (Show, Eq, Generic, NFDataX)
