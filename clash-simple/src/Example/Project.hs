-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

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
  -> Signal Dom30 Bool
  -> Signal Dom30 Bool
  -> Signal Dom30 Bool
  -> Signal Dom30 (BitVector 16)
  -> Signal Dom30
      (Bit, Bit, Bit, Bit, Bit, Unsigned 14, BitVector 16, Bit)
topEntity clk resetn btn1 btn2 btn3 rdata =
  postprocess <$>
  exposeClockResetEnable sam
    clk resetn enableGen
    btn1
    btn2
    btn3
    rdata
  where
    postprocess
      :: Outputs
      -> (Bit, Bit, Bit, Bit, Bit, Unsigned 14, BitVector 16, Bit)
    postprocess Outputs {..} =
      let (l1, l2, l3, l4, l5) =
            bitCoerce @(Unsigned 5) @(Bit, Bit, Bit, Bit, Bit) display
          (wen, wdata) =
            case ramWrite of
              Nothing -> (False, 0)
              Just v -> (True, v)
      in
      (l1, l2, l3, l4, l5, ramAddress, wdata, boolToBit wen)

--unpackVec5 . bitCoerce @(Unsigned 5) @(Vec 5 Bit) <$> 
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

                 , PortName "ram_rdata"
                 ]
    , t_output = PortProduct ""
                 [ PortName "led1"
                 , PortName "led2"
                 , PortName "led3"
                 , PortName "led4"
                 , PortName "led5"
                 , PortName "ram_addr"
                 , PortName "ram_wdata"
                 , PortName "ram_wen"
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

data Outputs = Outputs
  { ramAddress :: Unsigned 14
  , ramWrite :: Maybe (BitVector 16)
  , display :: Unsigned 5
  }
  deriving (Show, Eq, Generic, NFDataX)

data Mode
  = SetAddr
  | ReadValue
  | WriteValue (Unsigned 5)
  deriving (Show, Eq, Generic, NFDataX)

data State = State
  { mode :: Mode
  , address :: Unsigned 5
  , lastBtn1 :: Bool
  , lastBtn2 :: Bool
  , lastBtn3 :: Bool
  }
  deriving (Show, Eq, Generic, NFDataX)

sam
  :: (HiddenClockResetEnable dom)
  => Signal dom Bool -> Signal dom Bool -> Signal dom Bool -- buttons
  -> Signal dom (BitVector 16)
  -> Signal dom Outputs
sam bouncyBtn1 bouncyBtn2 bouncyBtn3 rdata =
  mealy f (State SetAddr 0 False False False) inp
  where
    f :: State -> (Bool, Bool, Bool, BitVector 16) -> (State, Outputs)
    f state@(State {..}) (btn1, btn2, btn3, rdata) =
      let displayRData = bitCoerce (resize @BitVector @16 @5 rdata)
          state' = state {lastBtn1 = btn1, lastBtn2 = btn2, lastBtn3 = btn3}
       in case mode of
            SetAddr | btn1 && not lastBtn1 ->
              ( state' {address = address + 1}
              , Outputs 0 Nothing $ address + 1
              )
            SetAddr | btn2 && not lastBtn2 ->
              ( state' {mode = WriteValue 0}
              , Outputs 0 Nothing 0
              )
            SetAddr | btn3 ->
              ( state' {mode = ReadValue}
              , Outputs (resize address) Nothing address
              )
            SetAddr ->
              ( state'
              , Outputs 0 Nothing address
              )
            ReadValue | not btn3 ->
              ( state' {mode = SetAddr}
              , Outputs 0 Nothing displayRData
              )
            ReadValue ->
              ( state'
              , Outputs (resize address) Nothing displayRData
              )
            WriteValue n | btn1 && not lastBtn1 ->
              ( state' {mode = WriteValue $ n + 1}
              , Outputs 0 Nothing n
              )
            WriteValue n | btn2 && not lastBtn2 ->
              ( state' {mode = SetAddr}
              , Outputs (resize address) (Just (resize (bitCoerce n))) n
              )
            WriteValue n ->
              ( state'
              , Outputs 0 Nothing n
              )
    inp =
      (,,,)
        <$> (debouncer (Proxy @10) bouncyBtn1)
        <*> (debouncer (Proxy @10) bouncyBtn2)
        <*> (debouncer (Proxy @10) bouncyBtn3)
        <*> rdata
