import           Criterion.Main
import           ImdbStats


main = defaultMain
  [ bgroup
      "scraping"
      [ bench "ema-links" $ nfIO $ scrapeWholeList "https://www.imdb.com/list/ls023670262/"
      , bench "100plus-links" $ nfIO $ scrapeWholeList "https://www.imdb.com/list/ls070418853/"
      , bench "ema-ratings" $ nfIO $ do
            links <- scrapeWholeList "https://www.imdb.com/list/ls023670262/"
            getAllRatings links
      ]
  ]
