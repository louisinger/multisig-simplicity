module Utils.Secp256k1 (
  isInf,
  curveOrder,
  g,
  pointX
) where

import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Spec (_z)

-- Checks if a Jacobian encoded point is the infinite one.
isInf :: GEJ -> Bool
isInf a = feIsZero (a^._z)

-- The secp256k1 curve order.
curveOrder :: Word256
curveOrder = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

-- Util function that convert a byteString to an Integer.
fromBytes :: ByteString -> Integer
fromBytes = foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

-- Extract the X component of Jacobian encoded point.
pointX :: GEJ -> Word256
pointX p = fromInteger . fromBytes . fromShort . fePack . normalize $ xcoord
  where
    xcoord = (p ^. _z) .*. sqr (inv (p ^. _z))

-- The generator point.
g :: GEJ
g = GEJ (unrepr 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798) (unrepr 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8) feOne