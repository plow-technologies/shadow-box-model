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


| -}

module Data.ShadowBox where


import Prelude (($),Int,fmap,(.),Bool (..),(&&),(||),not,maybe,(==),otherwise,(-),(+),(<),(>),(>=),(<=),IO
                ,(<$>),(<*>),either,const,id,(*),String
                ,show,(++),fromIntegral,Word,Either (..))
import Data.Bits
import Data.ShadowBox.Internal
import Data.Array.BitArray (BitArray,(!))
import qualified Data.Array.BitArray as BitArray
import qualified Data.Array.BitArray.ByteString as BitBS
import qualified Data.Bits.Bitwise as Bitwise
import Data.ByteString (ByteString)
import Data.Monoid

import qualified Data.ByteString as ByteString
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC



-- | Shadow Models are the shapes that are inserted into a world at an origin
newtype ShadowModel  = ShadowModel {_unshadowModel :: BitArray (Int,Int)}


showShadowModel (ShadowModel m) = fmap Bitwise.toListLE . ByteString.unpack . BitBS.toByteString $ m



-- | Build a  rectangle shadow of a given width and height
shadowRect :: Int -> Int -> ShadowModel
shadowRect width height = ShadowModel $ BitArray.fill ((0,0), (width,height) ) True










--------------------------------------------------
-- World
--------------------------------------------------

showWorld :: World -> [[Bool]]
showWorld (World m) = fmap Bitwise.toListLE . ByteString.unpack . BitBS.toByteString $ m


-- | World shadows are either created empty or are built up by inserting shadows
-- into them.  They are correct by construction because these are the only way to build them
newtype World = World { _unWorldShadow :: BitArray (Int,Int)}

val !?  ix = ba BitArray.!? ix
  where
    (World ba) = val

-- | Build a world with no shadows
emptyWorld :: Int -> Int -> World
emptyWorld width height = World $ BitArray.fill ((0,0), (width,height) ) False







-- | overlapping
-- if any bit is 1 inff both worlds, an intersection is reported as true

data Patchable = Patchable { shadow :: !ShadowModel
                           , world  :: !World}


overlapping :: Int -> Int -> ShadowModel -> World -> Either String Patchable
overlapping x y s@(ShadowModel sm) w@(World world) = _ -- findOverlap
  where
    upperXBoundOfTranslation = (shadowX + x)
    upperYBoundOfTranslation = shadowY + y  
    ((_,_) , (shadowX,shadowY)) = BitArray.bounds sm
    ((_,_),(width,height)) = BitArray.bounds world
    overlap = const  (BitArray.or $ transformedWorld ) <$> boundsCheck
    makePatchable = (const $ Patchable s w) <$> overlap 


    transformedWorld = BitArray.ixmap ((0,0),(shadowX,shadowY)) transform twobitArray
    transform i = maybe falseIdx readWorldValue (world BitArray.!? i)
      where
       readWorldValue val = if     val && (sm!i)
                              then trueIdx
                              else falseIdx


    boundsCheck
          | (upperXBoundOfTranslation > width) && (upperYBoundOfTranslation <= height) = Left $ "bounds exceeded"
          | (width <= 0) || (height <= 0) = Left "Max World must be greater than zero in both dimensions"
          | (x < 0) || (y < 0) = Left "Shadow coordinates must be greater than zero"
          | otherwise = Right ()


trueIdx = (0,0)
falseIdx = (0,1)
testRect3By3'  = shadowRect 3 3




twobitArray = BitArray.array (trueIdx,falseIdx) [(trueIdx, True), (falseIdx,False)]










-- | place a shadow model at a given position, assumes the model is represented in a square matrix
-- the matrix that is projected is actually just [0,1], it uses it as an intermediate while reading
-- values out of sm.  This allows us to control a true or false value without having to convert to a
-- list
addModelToWorld
  :: Int
     -> Int
     -> ShadowModel
     -> World
     -> Either String World
addModelToWorld x y (ShadowModel sm) (World world) = assembleWorld
  where
    ((_,_) , (shadowX,shadowY)) = BitArray.bounds sm
    ((_,_),(width,height)) = BitArray.bounds world

    upperXBoundOfTranslation = (shadowX + x)
    
    upperYBoundOfTranslation = shadowY + y        


    transform i = maybe falseIdx readShadowValue (sm BitArray.!?  (translate i) )

    
    translate (x',y')
      |(x' <   upperXBoundOfTranslation ) && (x' >= x) &&
       (y' < upperYBoundOfTranslation ) && (y' >= y) = (x' - x, y' - y)
      | otherwise = readWorldValue (x',y')-- force the bounds to be violated and return nothing


    readShadowValue val = if val
                             then trueIdx
                             else falseIdx
    readWorldValue i = if world!i
                       then trueIdx
                       else falseIdx




    -- If one of the values in the shadow space has a collission, don't build
--    intersectWorld = const (BitArray.ixmap (trueIdx, (width,height)) intersectTransform twobitArray) <$> boundsCheck

    assembleWorld = const (World $ BitArray.ixmap (trueIdx, (width, height)) transform twobitArray) <$> boundsCheck



    boundsCheck
      | (upperXBoundOfTranslation > width) && (upperYBoundOfTranslation <= height) = Left $ "bounds exceeded"
      | (width <= 0) || (height <= 0) = Left "Max World must be greater than zero in both dimensions"
      | (x < 0) || (y < 0) = Left "Shadow coordinates must be greater than zero"
      | otherwise = Right ()
-- Test for Add Model to empty world

testAddModelToEmptyWorld :: TestTree
testAddModelToEmptyWorld = testGroup "addModelToEmptyWorld tests" tests
  where
    tests = [ HU.testCase "test move correct" (testIndexesMatch False 3 3)
            , HU.testCase "test move correct" (testIndexesMatch True 4 4)
            , HU.testCase "test move correct" (testIndexesMatch True 5 5)
            , HU.testCase "test move correct" (testIndexesMatch True 6 6)
            , HU.testCase "test move correct" (testIndexesMatch False 7 7)
            , HU.testCase "test move correct" (testIndexesMatch False 0 0)]
            
    (Right (World worldWithModel)) = addModelToWorld 4 4 testRect3By3 (emptyWorld 10 10) 
    testRect3By3 = shadowRect 3 3
    testIndexesMatch b i j = (HU.assertEqual (show i <> " : " <> show j) b (indexesMatch i j))
    indexesMatch i j = (worldWithModel ! (i,j)) 



runTestAddModelToEmptyWorld :: IO ()
runTestAddModelToEmptyWorld = defaultMain testAddModelToEmptyWorld
