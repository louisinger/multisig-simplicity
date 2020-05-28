{-# LANGUAGE GADTs #-}

module MuSig where

import Prelude hiding (Word)

import Simplicity.Ty.Word (toWord256)
import Simplicity.Word (Word256)
import Simplicity.LibSecp256k1.Schnorr 
import Simplicity.Bitcoin.Programs.CheckSigHashAll (Lib(..), mkLib, checkSigHashAll, sigAllCMR, sigHashAll)
import Simplicity.Ty (TyC)
import Simplicity.Term.Core (Assert, Witness, (>>>), (&&&), witness)
import Simplicity.Bitcoin.Term (Primitive)
import Simplicity.Programs.Generic (scribe)
import Simplicity.Functor
import Simplicity.Tensor

import qualified Simplicity.Programs.Sha256 as Sha256
import qualified Simplicity.Programs.LibSecp256k1 as LibSecp256k1

import Utils.Scalar
import Utils.SchnorrMuSig


instance Show Sig where
  show (Sig r s) = "nonce = " ++ show r ++ "\nval = " ++ show s

-- checkDoubleSig takes two public keys as parameters and then verify a signature gi
checkDoubleSig :: (Assert term, Primitive term, Witness term, TyC a) => Lib term -> XOnlyPubKey -> XOnlyPubKey -> Sig -> term a ()
checkDoubleSig Simplicity.Bitcoin.Programs.CheckSigHashAll.Lib{sigAllCMR=sigAllCMR, sigHashAll=sigHashAll, checkSigHashAll=checkSigHashAll} p p' ~(Sig r s) 
  = (scrib &&& wit) >>> checkSigHashAll
  where
    scrib = scribe (toWord256 . toInteger $ x)
    wit = witness (toWord256 . toInteger $ r, toWord256 . toInteger $ s)
    (Just (XOnlyPubKey x)) = addPubKey p p'


lib :: (Assert term, Primitive term) => Lib term
lib = mkLib libSha256P libSecp256k1
 where
  libSha256P = Sha256.lib
  libSha256 = sndProduct `sfmap` libSha256P
  libSecp256k1 = LibSecp256k1.mkLib libSha256