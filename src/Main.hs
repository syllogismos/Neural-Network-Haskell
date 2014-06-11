{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import           "cassava" Data.Csv
import qualified Data.Vector          as V

main :: IO ()
main = do
    csvData <- BL.readFile "littletrain.csv"
    case decode False csvData of
        Left err -> putStrLn err
        Right (v :: V.Vector (V.Vector Double)) -> V.forM_ v $ \ name ->
            putStrLn $ show $ V.last name

