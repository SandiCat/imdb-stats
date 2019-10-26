{-# LANGUAGE QuasiQuotes #-}

module Main where

import ImdbStats
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import           Data.String.Interpolate
import System.Environment
import qualified Streamly

main = do
    [which] <- getArgs
    Just links <- Aeson.decode <$> LBS.readFile [i|data/list_#{which}.json|]
    writeStream [i|data/ratings_#{which}.json|] $ streamEncodeArray $ Streamly.asyncly $ getAllMovies links
