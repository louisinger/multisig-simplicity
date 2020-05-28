module Utils.Scalar (
  scalarModulus,
  scalarMod,
  addScalar,
  scalarToWord256,
  scalarMult
) where

import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Spec (Scalar(..))

-- Constant = the scalar modulus (< 2^256)
scalarModulus :: Word256
scalarModulus = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

-- Apply the mod scalarModulus to an integer
scalarMod :: Integer -> Integer
scalarMod v = v `mod` toInteger scalarModulus

-- Addition for two Scalar.
addScalar :: Scalar -> Scalar -> Scalar
addScalar (Scalar k) (Scalar k') = Scalar $ fromInteger (scalarMod $ toInteger k + toInteger k')

-- select the scalar's component
scalarToWord256 :: Scalar -> Word256
scalarToWord256 (Scalar v) = v

-- Returns the product (a point) of a scalar * a point.
scalarMult :: Scalar -> GEJ -> GEJ
scalarMult na a = ecMult a na scalarZero