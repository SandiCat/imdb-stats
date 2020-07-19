{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric #-}
{-# LANGUAGE  DeriveAnyClass #-}
{-# LANGUAGE  NamedFieldPuns #-}
{-# LANGUAGE  TypeSynonymInstances #-}
{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  StandaloneDeriving #-}
{-# LANGUAGE  StrictData #-}
{-# LANGUAGE  TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}


module ImdbStats where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                 as Text.Lazy
import qualified Data.Text.Read                as Text.Read
import           Text.HTML.Scalpel
import           Control.Monad
import           Data.Char                      ( isDigit )
import           Data.List.Extra                ( enumerate )
import           Data.Function                  ( (&) )
import           Debug.Trace
import           Data.List.Extra                ( intercalate )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Network.URL                   as URL
import           Data.String.Interpolate
import           GHC.Generics                   ( Generic )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Text                 as Aeson.Text
import           Data.Aeson.Types               ( Parser )
import           Control.Monad.Extra            ( fromMaybeM )
import           Control.Monad.Fail             ( MonadFail )
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Char8         as BS.Char8
import qualified Data.Attoparsec.ByteString.Char8
                                               as AP.BS
import           Control.DeepSeq                ( NFData )
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Data.String as DS
import qualified Data.Word as Word
import System.IO
import qualified Statistics.Test.StudentT as Stat
import qualified Statistics.Test.Types as Stat
import qualified Statistics.Sample as Stat
import Control.Applicative
import qualified Graphics.Rendering.Chart.Easy as Chart
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((/~), (/:), (=:))
import Data.Default
import qualified Data.List.Split as Split
import qualified Control.Retry as Retry


data Age = AllAges | LT18 | A18to29 | A30to44 | GT45 deriving (Show, Eq, Ord, Bounded, Enum, Generic, NFData, Aeson.ToJSON, Aeson.FromJSON)
data Gender = AllGenders | Male | Female deriving (Show, Eq, Ord, Bounded, Enum, Generic, NFData, Aeson.ToJSON, Aeson.FromJSON)

type Demographic = (Age, Gender)
-- TODO: rational or scientific instead of double
newtype Rating = Rating Double deriving (Show, Generic, NFData, Aeson.ToJSON, Eq, Aeson.FromJSON)
unRating (Rating x) = x -- TODO: so as not to mess with the avaliable JSON
newtype NumUsers = NumUsers Int deriving (Show, Generic, NFData, Aeson.ToJSON, Eq, Aeson.FromJSON)
data RatingStats = RatingStats {rating :: Rating, numUsers :: NumUsers} deriving (Generic, NFData, Aeson.ToJSON, Eq, Aeson.FromJSON)

instance Show RatingStats where
    show (RatingStats (Rating rating) (NumUsers numUsers)) =
        (show rating) ++ "@" ++ (show numUsers)

type RatingTable = Map.Map Demographic RatingStats

instance {-# OVERLAPS #-} Show RatingTable where
    show table = Map.toList table & map show & intercalate "\n"

a @. b = a @: [hasClass b]

tableToMap :: [[RatingStats]] -> RatingTable
tableToMap table = Map.fromList $ zip
    [ (age, gender)
    | gender <- enumerate :: [Gender]
    , age <- enumerate :: [Age]
    ]
    (mconcat table)

-- TODO: operator (//) implies arbitrarly deep nesting, i need direct nesting (children)

scrapeRatingTable :: Scraper BS.ByteString RatingTable
scrapeRatingTable =
    chroots ("td" @. "ratingTable") cell
        & chroots "tr"
        & fmap (tableToMap . tail) -- skip the first row, it's just headings (it will be an empty list)
        & chroots ("div" @: ["class" @= "title-ratings-sub-page"] // "table")
        >>= \case -- the second table is the relevant one
                _ : x : _ : [] -> return x
                _ -> fail "table isn't on the second spot"
  where
    cell :: Scraper BS.ByteString RatingStats
    cell = do
        rating <- text $ "div" @. "bigcell"
        numUsers <- text $ "div" @. "smallcell" // "a"

        -- TODO: maybe a monad transformer for this?
        case
                ( AP.BS.parseOnly AP.BS.double rating
                , BS.Char8.readInt $ BS.filter AP.BS.isDigit_w8 numUsers
                )
            of
                (Right parsedRating, Just (parsedNumUsers, _)) ->
                    return $ RatingStats (Rating parsedRating)
                                         (NumUsers parsedNumUsers)
                _ -> fail "can't parse cell"

scrapeMoviesFromListPage :: Scraper BS.ByteString (Vector URL.URL)
scrapeMoviesFromListPage =
    ("div" @. "lister-list" // "h3" @. "lister-item-header" // "a")
        & attrs "href"
        & fmap
              (Vector.fromList . catMaybes . fmap
                  (URL.importURL . BS.Char8.unpack)
              )

-- TODO: scrape the number of movies in a list, calculate the number of pages, scrape them in parallel
scrapeWholeList :: String -> IO (Vector URL.URL)
scrapeWholeList url = rec url 1 Vector.empty
  where
    rec :: String -> Int -> Vector URL.URL -> IO (Vector URL.URL)
    rec url page collected = do
        Just links <- scrapeURL
            (url ++ "?sort=list_order,asc&st_dt=&mode=detail&page=" ++ show page)
            scrapeMoviesFromListPage
        if Vector.null links
            then -- stop iterating when scraping fails (on 404)
                 return collected
            else rec url (page + 1) (collected <> links)

loadLinks :: FilePath -> IO [URL.URL]
loadLinks path = do
    Just links <- Aeson.decode <$> LBS.readFile path -- TODO mtl, handling errors?
    return links

newtype MovieId = MovieId { getMovieId :: Text.Text } deriving (Show)

extractId :: URL.URL -> MovieId
extractId = MovieId . Text.pack . (!! 1) . Split.splitOn "/" . URL.url_path

downloadAllMovies :: FilePath -> [MovieId] -> IO ()
downloadAllMovies folder =
    S.drain
    . S.mapM (print . fst)
    . S.indexed
    . S.asyncly
    . S.mapM saveMovie
    . S.maxRate 10
    . S.mapM downloadMovie
    . S.fromList
    where
        downloadMovie :: MovieId -> IO (MovieId, LBS.ByteString)
        downloadMovie movieId =
            Req.runReq 
                def { Req.httpConfigRetryPolicy = Retry.retryPolicy $ const $ Just 1000 } 
             $  (movieId,)
            <$> Req.responseBody
            <$> Req.req
                Req.GET
                (Req.https "www.imdb.com" /: "title" /: (getMovieId movieId) /: "ratings")
                -- TODO unite with proper url making
                Req.NoReqBody
                Req.lbsResponse
                ("ref_" =: ("tt_ov_rt" :: Text.Text))

        saveMovie :: (MovieId, LBS.ByteString) -> IO ()
        saveMovie (movieId, body) = 
            LBS.writeFile [i|#{folder}/#{getMovieId movieId}.html|] body -- TODO: path package

badRequestTest :: IO LBS.ByteString
badRequestTest =
    Req.runReq 
        def -- { Req.httpConfigRetryPolicy = Retry.retryPolicy $ const $ Just 1000 }
    $ Req.responseBody
    <$> Req.req
        Req.GET
        (Req.https "httpbin.org" /: "status" /: (Text.pack $ show 504))
        -- TODO unite with proper url making
        Req.NoReqBody
        Req.lbsResponse
        mempty

data Movie = Movie { movieURL :: URL.URL, ratingTable :: RatingTable }
    deriving (Show, Generic, NFData, Aeson.ToJSON, Aeson.FromJSON)

moviePageToRatingsPage :: URL.URL -> URL.URL
moviePageToRatingsPage (URL.URL url_type url_path url_params) =
    URL.URL url_type (url_path ++ "ratings") []

addImdbHost :: URL.URL -> URL.URL
addImdbHost url = url
    { URL.url_type = URL.Absolute
                         $ URL.Host (URL.HTTP True) "www.imdb.com" Nothing
    }

getMovie :: URL.URL -> IO (Maybe Movie)
getMovie url = do
    let ratingsURL = addImdbHost $ moviePageToRatingsPage url
    maybeMovie <- scrapeURL (URL.exportURL ratingsURL) scrapeRatingTable
    print url
    return (fmap (Movie ratingsURL) maybeMovie)

getAllMovies :: [URL.URL] -> S.AsyncT IO Movie
getAllMovies = S.mapMaybeM getMovie . S.fromList

getAllMoviesIO :: [URL.URL] -> IO [Movie]
getAllMoviesIO = S.toList . S.asyncly . getAllMovies

streamEncodeArray :: (S.MonadAsync m, Aeson.ToJSON a) => S.SerialT m a -> S.SerialT m Char
streamEncodeArray =
    S.cons '['
    . (flip (<>)) (S.yield ']')
    . S.concatMap (S.fromList . Text.Lazy.unpack)
    . S.intersperse ","
    . S.map Aeson.Text.encodeToLazyText

encodeMoviesStr :: [URL.URL] -> IO LBS.ByteString
encodeMoviesStr =
    (fmap LBS.pack) .S.toList . DS.encodeChar8 . streamEncodeArray . S.asyncly . getAllMovies

writeStream :: FilePath -> S.Serial Char -> IO ()
writeStream file stream = do
    fh <- openFile file WriteMode
    S.fold (FH.write fh) $ DS.encodeChar8 stream

instance Aeson.ToJSON URL.URL where
    toJSON = Aeson.String . Text.pack . URL.exportURL

instance Aeson.FromJSON URL.URL where
    parseJSON (Aeson.String str) = lift $ URL.importURL $ Text.unpack str
      where
-- TODO: there's gotta be a function that does this. mtl?
        lift :: (MonadFail m) => Maybe a -> m a
        lift (Just x) = return x
        lift Nothing = fail "got Nothing while trying to lift"
    parseJSON _ = fail "URL has to be a string"


deriving instance Generic URL.URL
instance NFData URL.URL
deriving instance Generic URL.URLType
instance NFData URL.URLType
deriving instance Generic URL.Host
instance NFData URL.Host
deriving instance Generic URL.Protocol
instance NFData URL.Protocol


loadMovies :: IO (Maybe (Vector Movie))
loadMovies = Aeson.decode <$> LBS.readFile "data/ratings_all.json"

mean v = Vector.sum v / (fromIntegral $ Vector.length v)

paired = Vector.mapMaybe $ \movie ->
    liftA2 (,) (genderedRating Male movie) (genderedRating Female movie)
    where genderedRating gender = ((fmap (unRating . rating)) . Map.lookup (AllAges, gender) . ratingTable)

genderDiff :: IO ()
genderDiff = do
    Just movies <- loadMovies
    print $ Stat.pairedTTest Stat.BGreater $ paired movies

plotGenderDiff :: IO ()
plotGenderDiff = do
    Just movies <- loadMovies
    Chart.toFile Chart.def "plots/scatter.png" $ do
        Chart.layout_title .= "Male x Female ratings"
        Chart.setColors [Chart.opaque Chart.blue]
        Chart.plot (Chart.points "points" $ Vector.toList $ paired movies)

data Range r
    = FromTo r r
    | LessThan r
    | MoreThan r

instance Show r => Show (Range r) where
    show (FromTo a b) = [i|#{a} - #{b}|]
    show (LessThan x) = [i|< #{x}|]
    show (MoreThan x) = [i|> #{x}|]

genderDiffHistogram :: IO ()
genderDiffHistogram = do
    Just movies <- loadMovies
    let diff = Vector.map (uncurry (-)) $ paired movies
        sd = Stat.stdDev diff
        m = Stat.mean diff
        

    putStrLn "hey"
