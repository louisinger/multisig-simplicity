module Utils.KeyGen (
  generateKeyPair,
  randomNumber,
  randomPrivateKey
) where

import Simplicity.LibSecp256k1.Schnorr
import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Spec (GEJ(..), Scalar(..))

import Utils.Secp256k1 (g, curveOrder, pointX)
import Utils.Scalar (scalarModulus, scalarMult)
import Utils.Ty (PrivateKey)

-- /!\ /!\ System.Random is not cryptographically secure! Todo: replace by Crypto.Random
import System.Random

-- Return PubKey from Word256 private key.
pubKeyFromPrivateKey :: Word256 -> XOnlyPubKey
pubKeyFromPrivateKey priv = XOnlyPubKey $ pointX pk
  where 
    pk = pkGEJ priv

-- Generate a random number (not secure !)
randomNumber :: (Integer, Integer) -> IO Integer
randomNumber (min, max) = toInteger . fst . randomR (min, max) <$> newStdGen

-- Generate a random number as private key for public key generation
randomPrivateKey :: IO PrivateKey
randomPrivateKey = randomNumber (0, toInteger scalarModulus)
  

-- Return the public key point for the private key scalar given as parameter.
pkGEJ :: Word256 -> GEJ
pkGEJ priv = scalarMult (Scalar priv) g

-- generate a new keypairs.
generateKeyPair :: IO (PrivateKey, XOnlyPubKey)
generateKeyPair = do 
  priv <- randomPrivateKey 
  return (fromInteger priv, pubKeyFromPrivateKey $ fromInteger priv)