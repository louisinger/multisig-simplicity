module Utils.SchnorrMuSig (
  sig,
  sign,
  addSig,
  addPubKey
) where

import Data.Maybe

import Simplicity.LibSecp256k1.Schnorr
import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Spec (pkPoint, sigUnpack, GEJ(..), FE(..), add, normalize)

import Utils.Scalar (scalarModulus, scalarMod, addScalar, scalarToWord256)
import Utils.Secp256k1 (pointX, fePackToWord256)
import Utils.KeyGen (randomNumber)
import Utils.Ty (PrivateKey)

-- add two pubkeys values to create a new one
addPubKey :: XOnlyPubKey -> XOnlyPubKey -> Maybe XOnlyPubKey
addPubKey pk pk' = do
  point <- pkPoint pk
  point' <- pkPoint pk'
  return $ XOnlyPubKey $ pointX (point <> point')

-- get a random nonce r, the private key priv and the message msg and then return the corresponding Sig.
sig :: Integer -> PrivateKey -> Word256 -> Sig
sig r priv msg = Sig (fromInteger . scalarMod $ r) (fromInteger val)
  where
    val = (r + priv * toInteger msg) `mod` toInteger scalarModulus

sign :: PrivateKey -> Word256 -> IO Sig
sign p msg = do
  r <- randomNumber (0, 2^256)
  return $ sig r p msg

-- add two Sig
addSig :: Sig -> Sig -> Maybe Sig
addSig s s' = do 
  (r, scal) <- sigUnpack s
  (r', scal') <- sigUnpack s'
  return $ Sig (fePackToWord256 . normalize $  r `add` r') (scalarToWord256 $ scal `addScalar` scal')
