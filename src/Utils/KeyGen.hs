module Utils.KeyGen (
  generateKeyPairs
) where

import Simplicity.LibSecp256k1.Schnorr
import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Spec (GEJ(..), Scalar(..))

import Utils.Secp256k1 (g, curveOrder, pointX)
import Utils.Scalar (scalarMod, scalarMult)

-- /!\ /!\ System.Random is not cryptographically secure! Todo: replace by Crypto.Random
import System.Random

-- Return PubKey from Word256 private key.
pubKeyFromPrivateKey :: Word256 -> XOnlyPubKey
pubKeyFromPrivateKey priv = XOnlyPubKey $ pointX pk
  where 
    pk = pkGEJ priv

-- Generate a random number as private key for public key generation
randomPrivateKey :: (Integer, Integer) -> IO Integer
randomPrivateKey (min, max) = scalarMod . toInteger . fst . randomR (min, max) <$> newStdGen

-- Return the public key point for the private key scalar given as parameter.
pkGEJ :: Word256 -> GEJ
pkGEJ priv = scalarMult (Scalar priv) g

-- generate a new keypairs.
generateKeyPairs :: IO (Integer, XOnlyPubKey)
generateKeyPairs = do 
  priv <- randomPrivateKey (0, toInteger curveOrder)
  return (fromInteger priv, pubKeyFromPrivateKey $ fromInteger priv)