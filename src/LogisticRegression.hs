
module LogisticRegression where

import Numeric.AD
import Numeric.AD.Types

import qualified Data.Vector as V
import Control.Monad
import Control.Monad.State
import Data.List
import Text.Printf

import Data.Traversable
import Data.Reflection

delta :: Floating a => a
delta = 1.0

gamma :: Double
gamma = 0.1

nIters :: Int
nIters = 4000

logit x = recip (1 + exp (negate x))

logLikelihood theta y x = y * log (logit z) + (1 - y) * log (1 - logit z)
  where
    z = V.sum $ V.zipWith (*) theta x

totalLogLikelihood :: (Floating a) => V.Vector a -> V.Vector a -> V.Vector (V.Vector a) -> a
totalLogLikelihood theta y x = (a - delta * b) / l
  where
    l = fromIntegral $ V.length y
    a = V.sum $ V.zipWith (logLikelihood theta) y x
    b = (/2) $ V.sum $ V.map (^2) theta

estimate :: (Floating b, Ord b) => V.Vector b -> V.Vector (V.Vector b) -> V.Vector b
estimate theta x = V.map (\xx -> predict $ logit (V.sum $ V.zipWith (*) theta xx)) x

success y yhat = correct/total
    where
        correct = fromIntegral $ V.length $ V.elemIndices True $ V.zipWith (==) y yhat
        total = fromIntegral $ V.length y

predict prob
    | prob > 0.5 = 1.0
    | otherwise = 0.0

-- estimates y x = gradientDescent $ \theta -> totalLogLikelihood theta (V.map auto y) (V.map (V.map auto) x)
train :: (Floating a, Ord a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector a -> Int -> V.Vector a
train y x init iter = last $ take iter $ gradientAscent (\theta -> totalLogLikelihood theta (V.map auto y) (V.map (V.map auto) x)) init



