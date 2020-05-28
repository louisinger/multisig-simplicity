# Multisig-Simplicity

## Description

MuSig implementation with [Simplicity](https://medium.com/chain-accelerator/i-tested-elements-simplicity-22c3189a725c).

The library is divided into several modules:
* The module `MuSig` contains the Simplicity script that check if a signature is valid for two given public keys.
* The module `Utils.SchnorrMuSig` contains all the expressions for manipulating keys and signature.
* The module `Utils.KeyGen` can be used to generate keypairs (not secure! only for testing)
* The module `Utils.Ty` defines all the types.
* The module `Utils.Secp256k1` implements usefull functions applying on secp256k's elements.
* The module `Utils.Scalar` provides utils for Scalar computations.
* The module `Test.DoubleSig` provides a bunch of data for testing purpose.

## Installation

1. Install the Simplicity library, on your device or inside a Nix environement: https://github.com/ElementsProject/simplicity.git (see the `Install` part for further details).
2. Then, move inside the cloned repo `multisig-simplicity` and launch `cabal update && cabal build`.
3. Finally, you can use `cabal repl` to test the library's functions.
