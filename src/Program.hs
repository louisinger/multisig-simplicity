module Program where

import Simplicity.Programs.LibSecp256k1
import Simplicity.Programs.Sha256

schnorrMuSigVerify :: (Core term) => term ((XOnlyPubKey, XOnlyPubKey), (Sig, Word256)) Bit