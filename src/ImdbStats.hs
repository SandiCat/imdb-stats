{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric #-}
{-# LANGUAGE  DeriveAnyClass #-}
{-# LANGUAGE  NamedFieldPuns #-}
{-# LANGUAGE  TypeSynonymInstances #-}
{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  StandaloneDeriving #-}
{-# LANGUAGE  StrictData #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}


module ImdbStats where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
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
import           Data.Aeson.Types               ( Parser )
import           Control.Monad.Extra            ( fromMaybeM )
import           Control.Monad.Fail             ( MonadFail )
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS.Char8
import qualified Data.Attoparsec.ByteString.Char8
                                               as AP.BS
import           Control.DeepSeq                ( NFData )
import qualified Control.Concurrent.Async      as Async


data Age = AllAges | LT18 | A18to29 | A30to44 | GT45 deriving (Show, Eq, Ord, Bounded, Enum, Generic, NFData, Aeson.ToJSON)
data Gender = AllGenders | Male | Female deriving (Show, Eq, Ord, Bounded, Enum, Generic, NFData, Aeson.ToJSON)

type Demographic = (Age, Gender)
-- TODO: rational or scientific instead of double
newtype Rating = Rating Double deriving (Show, Generic, NFData, Aeson.ToJSON, Eq)
newtype NumUsers = NumUsers Int deriving (Show, Generic, NFData, Aeson.ToJSON, Eq)
data RatingStats = RatingStats {rating :: Rating, numUsers :: NumUsers} deriving (Generic, NFData, Aeson.ToJSON, Eq)

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
        & fmap (\(_ : x : _ : []) -> x) -- the second table is the relevant one
  -- TODO: ^^ treba bacit neki maybe, nekako u monad strpat činjenicu da nekad ne prolazi
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


data Movie = Movie { movieURL :: URL.URL, ratingTable :: RatingTable } deriving (Show, Generic, NFData, Aeson.ToJSON)

moviePageToRatingsPage :: URL.URL -> URL.URL
moviePageToRatingsPage (URL.URL url_type url_path url_params) =
    URL.URL url_type (url_path ++ "ratings") []

addImdbHost :: URL.URL -> URL.URL
addImdbHost url = url
    { URL.url_type = URL.Absolute
                         $ URL.Host (URL.HTTP True) "www.imdb.com" Nothing
    }


getAllRatings :: Vector URL.URL -> IO (Vector Movie)
getAllRatings urls = Vector.mapMaybe id <$> Async.mapConcurrently f urls
  where
    f :: URL.URL -> IO (Maybe Movie)
    f url = do
        let ratingsURL = addImdbHost $ moviePageToRatingsPage url
        maybeMovie <- scrapeURL (URL.exportURL ratingsURL) scrapeRatingTable
        print url
        return (fmap (Movie ratingsURL) maybeMovie)


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
