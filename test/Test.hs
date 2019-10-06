import           Test.HUnit
import qualified Data.ByteString               as BS
import qualified Text.HTML.Scalpel             as Scalpel
import ImdbStats
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson

main = runTestTT $ TestList
    [ TestLabel "scrape ratings" $ TestCase $ do
        testHtml <- BS.readFile "data/example_ratings.html"
        -- TODO: parser won't let me split this list into multiple lines
        let expectedRatings = [((AllAges, AllGenders), 6.9, 253484), ((LT18, Female), 7.9, 266), ((A30to44, Female), 7.2, 13272)]

        let maybeTable = Scalpel.scrapeStringLike testHtml scrapeRatingTable
        -- TODO: MTL?
        case maybeTable of
          Just table -> do
              mapM_ 
                (\(key, rating, numUsers) -> 
                  assertEqual (show key) (Just $ RatingStats (Rating rating) (NumUsers numUsers)) $ Map.lookup key table)
                expectedRatings
          Nothing ->
            assertFailure "failed parsing table"
    , TestLabel "download and scrape ratings" $ TestCase $ do
        maybeTable <- Scalpel.scrapeURL
            "https://www.imdb.com/title/tt0111161/ratings?ref_=tt_ov_rt"
            scrapeRatingTable
        case maybeTable of
          Just table ->
            assertBool "table is non empty" $ length table > 0
          Nothing ->
            assertFailure "no table scraped"
    , TestLabel "download and scrape all movies in a list" $ TestCase $ do
        links <- scrapeWholeList "https://www.imdb.com/list/ls023670262/"
        assertBool "list of urls is not empty" $ length links > 0
        movies <- getAllRatings links
        assertBool "list of movies is not empty" $ length movies > 0
        mapM_ (assertBool "table not empty" . (>0) . length . ratingTable) movies
    , TestLabel "read and scrape all movies in a list" $ TestCase $ do 
        Just links <- Aeson.decode <$> LBS.readFile "data/list_ema.json"
        assertBool "list of urls is not empty" $ length links > 0
        movies <- getAllRatings links
        assertBool "list of movies is not empty" $ length movies > 0
        mapM_ (assertBool "table not empty" . (>0) . length . ratingTable) movies
    ]
