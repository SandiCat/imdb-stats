import           Criterion.Main
import           ImdbStats
import qualified Data.Vector as Vector


main = defaultMain
  [ bgroup
      "scraping"
      [ bench "ema-links" $ nfIO $ scrapeWholeList "https://www.imdb.com/list/ls023670262/"
      , bench "100plus-links" $ nfIO $ scrapeWholeList "https://www.imdb.com/list/ls070418853/"
      , bench "ema-ratings" $ nfIO $ do
            links <- Vector.toList <$> scrapeWholeList "https://www.imdb.com/list/ls023670262/"
            getAllMoviesIO links
      ]
  ]
