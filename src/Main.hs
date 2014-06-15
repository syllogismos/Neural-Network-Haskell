{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import           "cassava" Data.Csv
import qualified Data.Vector          as V

import LogisticRegression
import NeuralNetwork
import Numeric.AD
import Numeric.AD.Types

import System.Random

import Control.Monad

binary :: Double -> Double
binary 10.0 = 1.0
binary _ = 0.0


usingLogisticRegression :: String -> IO ()
usingLogisticRegression trainingFileName = do
    yxdata <- BL.readFile trainingFileName -- "smalltrain12.csv"
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

-- tootoo x = gradientDescent (\[t1,t2,t3] ->  (sum $ zipWith (*) [t1,t2,t3] $ map auto x))



usingNeuralNetwork :: String -> IO ()
usingNeuralNetwork trainingFileName = do
    yxData <- BL.readFile trainingFileName
    gen <- getStdGen
    let
        theta :: [Double] = take ((l1+1)*l2 + (l2+1)*l3) $ randomRs ((-0.1), 0.1) gen
        yx = decode False yxData :: Either String (V.Vector (V.Vector Double))
        yraw = fmap (V.map V.head) yx -- yraw = Right $ V.fromList [10.0,10.0,2.0] etc, and we need to convert this to our actual y,
        x = fmap (V.map V.tail) yx -- x = Right $ V.fromList [[][][]] you get the idea
        y = fmap (V.map getYBinary) yraw
        init = Right $ V.fromList theta
        iter = Right 10
        thetafinal = liftM4 trainNN init x y iter
        likelihoods = liftM2 predictLikelihood thetafinal x
    print "lol"

main :: IO ()
main = do
    theta <- generateTheta 4 2
    theta1 <- generateTheta 4 2
    let
        x = 2
    print theta
    print theta1