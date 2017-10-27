-- | Useful Clash utilities.
module Utils where

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

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(<&>) :: Applicative f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

(.^.) :: (Bits a, Applicative f) => f a -> f a -> f a
(.^.) = liftA2 xor

notA :: (KnownNat a, KnownNat b, Functor f) => f (BitVector a) -> f (BitVector b)
notA = fmap $ \x -> resize (complement (reduceOr x))

zero :: KnownNat n => BitVector n
zero = 0
