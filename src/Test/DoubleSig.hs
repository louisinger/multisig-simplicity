module Test.DoubleSig where

import Simplicity.LibSecp256k1.Schnorr 
import Simplicity.Word (Word256)

import Data.Maybe

import Utils.KeyGen (generateKeyPair)
import Utils.Ty (PrivateKey)
import Utils.SchnorrMuSig
import MuSig (checkDoubleSig)

keypair1 :: (PrivateKey, XOnlyPubKey)
keypair1 = (58984535928362690591825546060018799107679807110094510651334526420536838856906,XOnlyPubKey 48531400418450094083663381282543478753876055720179590447846973084425131421906)

r1 :: Integer
r1 = 93100591311139804898769750185928950855137899987556421060925815290202685624475

keypair2 :: (PrivateKey, XOnlyPubKey)
keypair2 = (16095338496839982124785433519597095187734869672371785769767590940793902931562,XOnlyPubKey 93907715445165933011296490495127297252189007135727681225447840407276186028421)

r2 :: Integer
r2 = 61040947540560050433376010022479862853465734771774500266190603489994615370139

muSigPubKey :: XOnlyPubKey
muSigPubKey = fromJust $ snd keypair1 `addPubKey` snd keypair2

message :: Word256
message = 0xFFFFFFFFFF

signature1 :: Sig
signature1 = sig r1 (fst keypair1) message 

signature2 :: Sig
signature2 = sig r2 (fst keypair2) message 

muSig :: Sig
muSig = fromJust $ signature1 `addSig` signature2

doubleSigScenario :: IO ()
doubleSigScenario = do
  putStrLn "1. Generate two keypairs"
  (priv1, pubkey1) <- generateKeyPair
  (priv2, pubkey2) <- generateKeyPair
  putStrLn ("[keys - user1]" ++ show (priv1, pubkey1))
  putStrLn ("[keys - user2]" ++ show (priv2, pubkey2))
  putStrLn "2. "

