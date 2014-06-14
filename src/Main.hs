{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import           "cassava" Data.Csv
import qualified Data.Vector          as V

import LogisticRegression
import NeuralNetwork
import Numeric.AD
import Numeric.AD.Types

import Control.Monad

binary :: Double -> Double
binary 10.0 = 1.0
binary _ = 0.0


lel :: IO ()
lel = do
    yxdata <- BL.readFile "smalltrain12.csv"
    let
        yx = decode False yxdata :: Either String (V.Vector (V.Vector Double))
        y = fmap (V.map (binary . V.head)) yx
        x = fmap (V.map V.tail) yx
        -- l = fmap (V.length . V.head) x
        init = Right $ V.fromList $ replicate 400 0.0
        iter = Right 10
    print "training, kooo chok chok chok"
    let
        final = liftM4 train y x init iter
        estimated = liftM2 estimate final x
        winpercent = liftM2 success estimated y
        estimated0 = liftM2 estimate init x
        winpercent0 = liftM2 success estimated0 y
    print winpercent
    print winpercent0

tootoo x = gradientDescent (\[t1,t2,t3] ->  (sum $ zipWith (*) [t1,t2,t3] $ map auto x))

main :: IO ()
main = do
    theta <- generateTheta 4 2
    let
        x = 2
    print theta