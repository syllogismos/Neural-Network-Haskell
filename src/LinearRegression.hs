module LinearRegression where

import Numeric.AD
import Numeric.AD.Types

import qualified Data.Vector as V

yhat :: (Floating a) => V.Vector a -> V.Vector a -> a
yhat theta x = V.sum $ V.zipWith (*) theta x


singleCost :: (Floating a) => V.Vector a -> a -> V.Vector a -> a
singleCost theta y x = 0.5 * (y - yhat theta x)^2


totalCost :: (Floating a) => V.Vector a -> V.Vector a -> V.Vector (V.Vector a) -> a
totalCost theta y x = (/l) $ V.sum $ V.zipWith (singleCost theta) y x
    where l = fromIntegral $ V.length y


finalTheta :: (Floating a, Ord a) => V.Vector a -> V.Vector a -> V.Vector (V.Vector a) -> Int -> V.Vector a
finalTheta init y x iter = last $ take iter $ gradientDescent (\theta -> totalCost theta (V.map auto y) (V.map (V.map auto) x)) init