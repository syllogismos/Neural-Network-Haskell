{-# LANGUAGE RankNTypes #-}
module NeuralNetwork where

import Numeric.AD
import Numeric.AD.Types
import qualified Data.Vector as V
import System.Random

import Control.Monad
import Data.Monoid

inputLayer = 200 :: Int
hiddenLayer = 20 :: Int
outputLayer = 10 :: Int

logit x = recip (1 + exp (-x))^2

-- input layer 200 nodes
-- hidden layer 20 nodes
-- final layer 10 nodes

nextLayer :: (Floating a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector a
nextLayer layer theta = V.map (logit . V.sum . V.zipWith (*) layer') theta
     where
        layer' = V.cons 1.0 layer

htheta :: (Floating a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> V.Vector a
htheta x theta1 theta2 = nextLayer hidden theta2
    where
        hidden = nextLayer x theta1

fun :: (Floating a) => Int -> [a] -> [[a]]
fun n = map (take n) . takeWhile (not . null) . iterate (drop n)

generateTheta :: Int -> Int -> IO (V.Vector (V.Vector Double))
generateTheta l1 l2 = do
    gen <- getStdGen
    let
        mat = take l2 $ fun l1 $ randomRs ((-0.1), 0.1) gen
    return $ V.fromList $ map V.fromList mat

logLikelihood :: (Floating a) => a -> a -> a
logLikelihood y h = y * (logit h) + (1 - y) * logit(1 - h)

singleCost :: (Floating a) => V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> V.Vector a -> V.Vector a -> a
singleCost theta1 theta2 y x = V.sum $ V.zipWith logLikelihood final y
    where
        final = htheta x theta1 theta2

totalCost :: (Floating a) => V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> a
totalCost theta1 theta2 y x = (/l) $ V.sum $ V.zipWith (\xx yy -> singleCost theta1 theta2 yy xx ) y x
    where
        l = fromIntegral $ V.length y

trainNN :: (Floating a, Ord a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> Int -> V.Vector a
trainNN init x y iter = last $ take iter $ gradientDescent (\theta -> totalCost (fst $ unroll theta) (snd $ unroll theta) (V.map (V.map auto) y) (V.map (V.map auto) x)) init


-- given theta and x predict y, just the probabilities? or the actual values.
predictLikelihood :: (Floating a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
predictLikelihood theta x = V.map (\temp -> htheta temp theta1 theta2) x
    where
        (theta1, theta2) = unroll theta

-- functions that roll and unroll the theta, matrix to vector and vector to matrix conversion functions.
unroll :: (Floating a) => V.Vector a -> (V.Vector (V.Vector a), V.Vector (V.Vector a))
unroll theta = ((temp $ take l2 $ fun (l1+1) theta'), (temp $ take l3 $ fun (l2+1) theta''))
    where
        theta' = V.toList theta
        temp x = V.fromList $ map V.fromList x
        theta'' = drop ((l1+1)*l2) theta'

roll :: (Floating a) => V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> V.Vector a
roll theta1 theta2 =  (join theta1) <> (join theta2)


-- hardcoded the number of nodes in the neural network in each layer
l1 = 200
l2 = 20
l3 = 10