
{- |
Module      : Data.ShadowBoxSpec
Description : Tests for shadowbox
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

A quick note...
These tests access the internals of the smart constructors directly, this is highly unadvisable.
THough available as an option

| -}

module Data.ShadowBoxSpec (tests) where

import Data.ShadowBox

import Test.Tasty
import qualified Test.Tasty.HUnit as HU

import Data.ShadowBox.Internal (World(World))


import Data.Monoid
import Data.Array.BitArray ((!))

tests :: TestTree
tests = testGroup "Tests" [testAddModelToEmptyWorld, testMakePatchable]



--------------------------------------------------
-- Tests
--------------------------------------------------


-- Test for Add Model to empty world

testAddModelToEmptyWorld :: TestTree
testAddModelToEmptyWorld = testGroup "addModelToEmptyWorld tests" tests'
  where
    tests' = [ HU.testCase "test move correct" (testIndexesMatch False 3 3)
             , HU.testCase "test move correct" (testIndexesMatch True 4 4)
             , HU.testCase "test move correct" (testIndexesMatch True 5 5)
             , HU.testCase "test move correct" (testIndexesMatch True 6 6)
             , HU.testCase "test move correct" (testIndexesMatch False 7 7)
             , HU.testCase "test move correct" (testIndexesMatch False 0 0)]

    ((World worldWithModel)) = addPatchToWorld patchAndWorld
      where
        (Right  patchAndWorld ) = makePatchable 4 4 testRect3By3 (emptyWorld 10 10) 
    testIndexesMatch b i j = (HU.assertEqual (show i <> " : " <> show j) b (indexesMatch i j))
    indexesMatch i j = (worldWithModel ! (i,j)) 



-- runTestAddModelToEmptyWorld :: IO ()
-- runTestAddModelToEmptyWorld = defaultMain testAddModelToEmptyWorld


testRect3By3 :: ShadowModel
testRect3By3  = shadowRect 3 3



-- Patch Tests
-- runTestMakePatchable :: IO ()
-- runTestMakePatchable = defaultMain testMakePatchable


-- stringTest :: Int -> Int -> Either String String
-- stringTest i j = showWorld <$> (addModelToWorld i j testRect3By3 w)
--   where
--     (Right w) = (testWorldWithRect (4::Int) (4::Int) )


-- testWorldWithRect :: t -> t1 -> Either String World
-- testWorldWithRect _ _ = (addPatchToWorld <$> (makePatchable 4 4 testRect3By3 (emptyWorld 10 10)))



testMakePatchable :: TestTree
testMakePatchable  = testGroup "makePatchable tests" tests'
  where
    tests' = [ HU.testCase "a ShadowMode that is too big is rejected" tooBigShadow
             , HU.testCase "      patch something when there is space"           (intersectionTests id  0 0 )
             , HU.testCase "don't patch something when there is space"           (intersectionTests id  1 1 )
             , HU.testCase "don't patch something when there is an intersection" (intersectionTests id 2 2 )
             , HU.testCase "don't patch something when there is an intersection" (intersectionTests not 3 3 )
             , HU.testCase "don't patch something when there is an intersection" (intersectionTests not 4 4 )
             , HU.testCase "don't patch something when there is an intersection" (intersectionTests not 5 5 )            
             , HU.testCase "don't patch something when there is an intersection" (intersectionTests not 6 6 )
             , HU.testCase "don't patch something when there is an intersection" (intersectionTests id 7 7 )
             , HU.testCase "don't patch something when there is a boundary" (intersectionTests not 8 8 )
             , HU.testCase "don't patch something when there is a boundary" (intersectionTests not 9 9 )
             ]

    tooBigShadow = HU.assertBool "3x3 model, 1 x 3 world" $ either (const True) (const False) $ makePatchable 0 0 testRect3By3 (emptyWorld 1 3)


    isRight = either (const False) (const True)
    (Right worldWithRectangle ) = addPatchToWorld <$> (makePatchable 4 4 testRect3By3 (emptyWorld 10 10))
    intersectionTests f i j = HU.assertBool  ("x=" <> show i <> " y=" <> show j) $ f $ isRight $ makePatchable i j testRect3By3 worldWithRectangle 
