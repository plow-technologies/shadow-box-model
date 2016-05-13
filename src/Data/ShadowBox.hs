{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Module      : Data.ShadowBox
Description : Detect collissions with multiple items at the same time by keeping a world model
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

Normal collision detection proceeds by checking one model against another,  one at a time.

If you are dealing with a discretized grid you can simulataneously check collissions against
all models simultaneously by projecting your models into a Binary grid.
Then by looking for an empty intersection set, collissions are avoided and a new world is returned.

Algorithmic complexity is determined by the grid size only.
A Grid can be stored in (GridSize/8) Bytes

the theory is to create a Shadow model, that is a rectangular projection into bits.

Then you position that projection onto a World, which is a 2d bit array.

you can do this all in 1 step with



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




The interfaces presented here have smart constructors and try to keep you safe..
however, you can go to the internal package which is exposed if you want to violate the model assumptions, you bad person you



| -}

module Data.ShadowBox  ( makePatchable
                       , emptyWorld
                       , addPatchToWorld
                       , addModelToWorld
                       , showWorld
                       , showShadowBoxModel
                       , shadowRect
                       , ShadowModel
                       , World
                       ) where

import Data.ShadowBox.Internal



