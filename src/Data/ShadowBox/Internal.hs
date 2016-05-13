{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Module      : Data.ShadowBox.Internal
Description : Detect collissions with multiple items at the same time by keeping a world model
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

Normal collision detection proceeds by checking one model against another,  one at a time.

If you are dealing with a discretized grid you can simulataneously check collissions against
all models simultaneously by projecting your models into a Binary grid.
Then by looking for an empty intersection set, collissions are avoided and a new world is returned.

Algorithmic complexity is determined by the grid size only.

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




| -}
module Data.ShadowBox.Internal  where


import Debug.Trace (traceShow)
import Prelude (($),Int,fmap,(.),Bool (..),(&&),(||),not,maybe,(==),otherwise,(-),(+),(<),(>),(>=),(<=),IO
                ,(<$>),(<*>),either,const,id,(*),String,(=<<),putStrLn,zip,fail
                ,show,(++),fromIntegral,Word,Either (..))
import Data.String (unlines)       
import Data.Bits
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
-- Enter the width and height in bits
shadowRect :: Int -> Int -> ShadowModel
shadowRect width height = ShadowModel $ BitArray.fill ((0,0), (width, height ) ) True













--------------------------------------------------
-- World
--------------------------------------------------

showWorld :: World -> String
showWorld (World m) = mconcat $ convertDirectly
  where
    ((_,_),(maxX,maxY)) = BitArray.bounds m
    convertToChar x y c = case c of
                            True -> " " <> "X" <> " " <> finish
                            False -> " " <> "_" <> " " <> finish
        where
          finish
            |y == maxY = "\n" 
            |otherwise = ""



    convertDirectly = [convertToChar x y (m!(x,y)) | x <-[0..maxX] , y <- [0.. maxY]]
-- | World shadows are either created empty or are built up by inserting shadows
-- into them.  They are correct by construction because these are the only way to build them
newtype World = World { _unWorldShadow :: BitArray (Int,Int)}

val !?  ix = ba BitArray.!? ix
  where
    (World ba) = val



-- | Build a world with no shadows
-- The width and height are in pixel length
emptyWorld :: Int -> Int -> World
emptyWorld width height = World $ BitArray.fill ((0,0), (width - 1 ,height -1 ) ) False







-- | overlapping
-- if any bit is 1 inff both worlds, an intersection is reported as true
data Patchable = Patchable {
            _ix :: !Int
         ,  _iy :: !Int
         ,  _shadow :: ShadowModel
         ,  _world  :: World}



-- | Make a patchable world, grouping world and model together
-- This runs all the boundary tests so that patches can be applied quickly

makePatchable :: Int -> Int -> ShadowModel -> World -> Either String Patchable
makePatchable x y s@(ShadowModel sm) w@(World world) = makePatchableFinal
  where
    upperXBoundOfTranslation = (shadowX + x)

    upperYBoundOfTranslation = shadowY + y

    ((_,_) , (shadowX,shadowY)) = BitArray.bounds sm

    ((_,_) , (maxWorldX,maxWorldY)) = BitArray.bounds world
    width = maxWorldX + 1
    height = maxWorldY + 1

    overlap = const  ( BitArray.or $ transformedWorld ) <$> boundsCheck

    eoverlap
      | (Right True) == overlap = Left "Overlap found"
      | otherwise = overlap

    makePatchableFinal = (const $ Patchable x y s w) <$> eoverlap 

    transformedWorld = BitArray.ixmap ((0,0),(shadowX,shadowY)) transform twobitArray

    transform i@(x',y') = maybe falseIdx readWorldValue ( world BitArray.!?  ((x - 1) + x',(y - 1) + y') )
      where       
       readWorldValue val =  if     val && (sm!i)
                             then   trueIdx
                             else   falseIdx


    boundsCheck
          | (upperXBoundOfTranslation > width) || (upperYBoundOfTranslation > height) = Left $ "bounds exceeded upperX:" <> (show upperXBoundOfTranslation) <> " width:" <> (show width) <>
                                                                                                   "bounds exceeded upperY:" <> (show upperYBoundOfTranslation) <> " height:" <> (show height)


          | (width <= 0) || (height <= 0) = Left "Max World must be greater than zero in both dimensions"
          | (x < 0) || (y < 0) = Left "Shadow coordinates must be greater than zero"
          | (x > width) || (y > height) = Left $ "x must be less than " <> (show width) <> " y less than " <> (show height)
          | otherwise = Right ()





testMakePatchable  = testGroup "makePatchable tests" tests
  where
    tests = [ HU.testCase "a ShadowMode that is too big is rejected" tooBigShadow
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

    emptyWorldPatchTest (i'::Word) (j' :: Word) = makeTestPatch
      where
         makeTestPatch = isRight $ makePatchable  i j testRect3By3 (emptyWorld imax jmax)
         i = 30 -- fromIntegral i' + 10  
         j = 30 -- fromIntegral j' + 10 
         imax = 300
         jmax = 300


    isRight = either (const False) (const True)
    (Right worldWithRectangle ) = addPatchToWorld <$> (makePatchable 4 4 testRect3By3 (emptyWorld 10 10))
    intersectionTests f i j = HU.assertBool  ("x=" <> show i <> " y=" <> show j) $ f $ isRight $ makePatchable i j testRect3By3 worldWithRectangle 



addModelToWorld :: Int -> Int -> ShadowModel -> World -> Either String World
addModelToWorld x y sm w = addPatchToWorld <$> makePatchable x y sm w





runTestMakePatchable = defaultMain testMakePatchable


stringTest i j = showWorld <$> (addModelToWorld i j testRect3By3 w)
  where
    (Right w) = (testWorldWithRect 4 4 )
testWorldWithRect i j = (addPatchToWorld <$> (makePatchable 4 4 testRect3By3 (emptyWorld 10 10)))





{-- 
testCheckIntersection :: TestTree
testCheckIntersection = testGroup "makePatchable tests" tests
  where
    tests = [ QC.testProperty "empty worlds are empty after intersection"  testEmpty
            , QC.testProperty "intersection of a world with itself is a collission" testIdentityCollission]    

--    testEmpty :: Int -> Int -> Bool
--    testEmpty = (\i j -> not $ checkIntersection (emptyWorld i j) (emptyWorld i j) )

    worldWithRect  = addPatchToWorld 4 4 testRect3By3 (emptyWorld 10 10)
    testRect3By3 = shadowRect 3 3

    testIdentityCollission (i'::Word) (j'::Word) = either (const False) (id) $ checkIntersection <$>
                                                                               (addPatchToWorld i j testRect3By3 (emptyWorld imax jmax) ) <*> 
                                                                               (addPatchToWorld i j testRect3By3 (emptyWorld imax jmax) )  
        where
         i = fromIntegral i' + 10  
         j = fromIntegral j' + 10 
         imax = 2* i
         jmax = 2 * j

runTestCheckIntersection = defaultMain testCheckIntersection      

--}








 -- Patch util functions
trueIdx = (0,0)
falseIdx = (0,1)
testRect3By3  = shadowRect 3 3




twobitArray = BitArray.array (trueIdx,falseIdx) [(trueIdx, True), (falseIdx,False)]










-- | place a shadow model at a given position, assumes the model is represented in a square matrix
-- the matrix that is projected is actually just [0,1], it uses it as an intermediate while reading
-- values out of sm.  This allows us to control a true or false value without having to convert to a
-- list
addPatchToWorld  :: Patchable
     -> World
addPatchToWorld (Patchable x y (ShadowModel sm) (World world)) = assembleWorld
  where
    ((_,_) , (shadowX,shadowY)) = BitArray.bounds sm
    ((_,_),(width,height)) = BitArray.bounds world

    upperXBoundOfTranslation = (shadowX + x)

    upperYBoundOfTranslation = shadowY + y        


    transform i = maybe (readWorldValue i) readShadowValue (sm BitArray.!?  (translate i) )


    assembleWorld = (World $ BitArray.ixmap ((0,0), (width, height)) transform twobitArray)

    translate (xFromWorld,yFromWorld)
      |(xFromWorld <   upperXBoundOfTranslation ) && (xFromWorld >= x) &&
       (yFromWorld < upperYBoundOfTranslation ) && (yFromWorld >= y) = (xFromWorld - x, yFromWorld - y)
      | otherwise = (shadowX + 1, shadowY + 1)-- force the bounds to be violated and return nothing


    readShadowValue val = if val
                             then trueIdx
                             else falseIdx

    readWorldValue i = if world!i
                       then trueIdx
                       else falseIdx







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

    ((World worldWithModel)) = addPatchToWorld patchAndWorld
      where
        (Right  patchAndWorld ) = makePatchable 4 4 testRect3By3 (emptyWorld 10 10) 
    testIndexesMatch b i j = (HU.assertEqual (show i <> " : " <> show j) b (indexesMatch i j))
    indexesMatch i j = (worldWithModel ! (i,j)) 



runTestAddModelToEmptyWorld :: IO ()
runTestAddModelToEmptyWorld = defaultMain testAddModelToEmptyWorld
