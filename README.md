# shadow-box-model

shadow box is a tool for 2d collission detection on rectangular grids.  
## Installation

shadow-box-model can be installed with cabal install or cabal install --ghcjs
## Usage
``` haskell
>>> either (fail) (putStrLn. showWorld) $ addModelToWorld 7 7 (shadowRect 3 3) (emptyWorld 10 10)
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  X  X  X 
 _  _  _  _  _  _  _  X  X  X 
 _  _  _  _  _  _  _  X  X  X 

>>> let (Right world1) = addModelToWorld 7 7 (shadowRect 3 3) (emptyWorld 10 10)

>>> let (Right world2) = addModelToWorld 1 1 (shadowRect 3 3) world1


>>> putStrLn . showWorld $ world2
 _  _  _  _  _  _  _  _  _  _ 
 _  X  X  X  _  _  _  _  _  _ 
 _  X  X  X  _  _  _  _  _  _ 
 _  X  X  X  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  _  _  _ 
 _  _  _  _  _  _  _  X  X  X 
 _  _  _  _  _  _  _  X  X  X 
 _  _  _  _  _  _  _  X  X  X 

```

Also, the tests show nice use cases

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

