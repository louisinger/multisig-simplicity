{-# LANGUAGE GADTs #-}

module MuSig where

import Prelude hiding (Word)

import Simplicity.Ty.Word (Word, Vector(..), word256, toWord256, fromWord256)
import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Schnorr 
import Simplicity.Programs.LibSecp256k1 (schnorrAssert)
import Simplicity.LibSecp256k1.Spec
import Simplicity.Bitcoin.Programs.CheckSigHashAll (Lib(..), checkSigHashAll, sigAllCMR, sigHashAll)
import Simplicity.Ty (TyC)
import Simplicity.Term.Core (Assert, Witness, (>>>), (&&&), witness)
import Simplicity.Bitcoin.Term (Primitive)
import Simplicity.Programs.Generic
import Lens.Family2 ((^.), (^..))
import Data.ByteString.Short (fromShort)
import Data.ByteString (ByteString, foldl')
import Data.Bits (shiftL, (.|.))
import Control.Monad (guard)

import System.Random

-- Show instance used to write Sig expressions.
instance Show Sig where
  show (Sig x y) = show x ++ "\n" ++ show y

isInf :: GEJ -> Bool
isInf a = feIsZero (a^._z)

scalarModulus :: Word256
scalarModulus = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

scalarMod :: Integer -> Integer
scalarMod v = v `mod` toInteger scalarModulus

addScalar :: Scalar -> Scalar -> Scalar
addScalar (Scalar k) (Scalar k') = Scalar $ fromInteger (scalarMod $ toInteger k + toInteger k')

scalarToWord256 :: Scalar -> Word256
scalarToWord256 (Scalar v) = v

-- static data
curveOrder = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

order = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
pubkey = XOnlyPubKey 0x00000000000000000000003b78ce563f89a0ed9414f5aa28ad0d96d6795f9c63
k = order `div` 2
priv = order `div` 2
r = 0x00000000000000000000003b78ce563f89a0ed9414f5aa28ad0d96d6795f9c63 :: Word256

g :: GEJ
g = GEJ (unrepr 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798) (unrepr 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8) feOne

-- signature
sampleSig :: Sig
sampleSig = sig r k order k 100

-- Generate a random number
randomPrivateKey :: (Integer, Integer) -> IO Integer
randomPrivateKey (min, max) = scalarMod . toInteger . fst . randomR (min, max) <$> newStdGen

scalarMult :: Scalar -> GEJ -> GEJ
scalarMult na a = ecMult a na scalarZero

-- Return the public key point for the private key scalar given as parameter.
pkGEJ :: Word256 -> GEJ
pkGEJ priv = scalarMult (Scalar priv) g

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

-- Return PubKey from Word256 private key.
pubKeyFromPrivateKey :: Word256 -> XOnlyPubKey
pubKeyFromPrivateKey priv = XOnlyPubKey $ pointX pk
  where 
    pk = pkGEJ priv

-- generate a new keypairs.
generateKeyPairs :: IO (Integer, XOnlyPubKey)
generateKeyPairs = do 
  priv <- randomPrivateKey (0, order)
  return (fromInteger priv, pubKeyFromPrivateKey $ fromInteger priv)

-- convert Simplicity's Word to a string.
writeWord :: Word a -> a -> String
writeWord SingleV (Left ()) = "0"
writeWord SingleV (Right ()) = "1"
writeWord (DoubleV w) (v1, v2) = writeWord w v1 ++ writeWord w v2

-- writeWord for Simplicity's Word256 implementation.
writeWord256 = writeWord word256

-- calculate
sig :: Word256 -> Integer -> Integer -> Integer -> Integer -> Sig
sig r k order priv msg = Sig r (fromInteger val)
  where
    val = (k + priv * msg) `mod` order

-- add two sig with same r
addSig :: Sig -> Sig -> Maybe Sig
addSig s s' = do 
  (r, scal) <- sigUnpack s
  (r', scal') <- sigUnpack s'
  return $ Sig rs (scalarToWord256 $ scal `addScalar` scal')
  where
    (Sig rs _) = s

-- add two pubkeys values to create a new one
addPubKey :: XOnlyPubKey -> XOnlyPubKey -> Maybe XOnlyPubKey
addPubKey pk pk' = do
  point <- pkPoint pk
  point' <- pkPoint pk'
  return $ XOnlyPubKey $ pointX (point <> point')

-- checkDoubleSig takes two public keys as parameters and then verify a signature gi
checkDoubleSig :: (Assert term, Primitive term, Witness term, TyC a) => Lib term -> XOnlyPubKey -> XOnlyPubKey -> Sig -> term a ()
checkDoubleSig Simplicity.Bitcoin.Programs.CheckSigHashAll.Lib{sigAllCMR=sigAllCMR, sigHashAll=sigHashAll, checkSigHashAll=checkSigHashAll} p p' ~(Sig r s) 
  = (scrib &&& wit) >>> checkSigHashAll
  where
    scrib = scribe (toWord256 . toInteger $ x)
    wit = witness (toWord256 . toInteger $ r, toWord256 . toInteger $ s)
    (Just (XOnlyPubKey x)) = addPubKey p p'
