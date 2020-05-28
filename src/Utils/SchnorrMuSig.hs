module Utils.SchnorrMuSig (
  sig,
  addSig,
  addPubKey
) where

import Simplicity.LibSecp256k1.Schnorr
import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Spec (pkPoint, sigUnpack, GEJ(..))

import Utils.Scalar (scalarModulus, addScalar, scalarToWord256)
import Utils.Secp256k1 (pointX)

-- add two pubkeys values to create a new one
addPubKey :: XOnlyPubKey -> XOnlyPubKey -> Maybe XOnlyPubKey
addPubKey pk pk' = do
  point <- pkPoint pk
  point' <- pkPoint pk'
  return $ XOnlyPubKey $ pointX (point <> point')

-- sign a message
sig :: Word256 -> Integer -> Integer -> Integer -> Integer -> Sig
sig r k order priv msg = Sig r (fromInteger val)
  where
    val = (k + priv * msg) `mod` toInteger scalarModulus

-- add two sig with same r
addSig :: Sig -> Sig -> Maybe Sig
addSig s s' = do 
  (r, scal) <- sigUnpack s
  (r', scal') <- sigUnpack s'
  return $ Sig rs (scalarToWord256 $ scal `addScalar` scal')
  where
    (Sig rs _) = s