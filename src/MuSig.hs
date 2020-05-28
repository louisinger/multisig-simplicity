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

-- static data
order = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
pubkey = XOnlyPubKey 0x00000000000000000000003b78ce563f89a0ed9414f5aa28ad0d96d6795f9c63
k = order `div` 2
priv = order `div` 2
r = 0x00000000000000000000003b78ce563f89a0ed9414f5aa28ad0d96d6795f9c63 :: Word256

-- signature
sampleSig :: Sig
sampleSig = sig r k order k 100

-- checkDoubleSig takes two public keys as parameters and then verify a signature gi
checkDoubleSig :: (Assert term, Primitive term, Witness term, TyC a) => Lib term -> XOnlyPubKey -> XOnlyPubKey -> Sig -> term a ()
checkDoubleSig Simplicity.Bitcoin.Programs.CheckSigHashAll.Lib{sigAllCMR=sigAllCMR, sigHashAll=sigHashAll, checkSigHashAll=checkSigHashAll} p p' ~(Sig r s) 
  = (scrib &&& wit) >>> checkSigHashAll
  where
    scrib = scribe (toWord256 . toInteger $ x)
    wit = witness (toWord256 . toInteger $ r, toWord256 . toInteger $ s)
    (Just (XOnlyPubKey x)) = addPubKey p p'
