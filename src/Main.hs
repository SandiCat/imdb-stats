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


module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import Text.HTML.Scalpel
import Control.Monad
import Data.Char (isDigit)
import Data.List.Extra  (enumerate)
import Data.Function ((&))
import Debug.Trace
import Data.List.Extra (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Network.URL as URL
import Data.String.Interpolate
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Fail (MonadFail)
import qualified Data.Vector as Vector
import Data.Vector (Vector)

data Age = AllAges | LT18 | A18to29 | A30to44 | AGT45 deriving (Show, Eq, Ord, Bounded, Enum)
data Gender = AllGenders | Male | Female deriving (Show, Eq, Ord, Bounded, Enum)

type Demographic = (Age, Gender)
newtype Rating = Rating Float deriving (Show)
newtype NumUsers = NumUsers Int deriving (Show)
data RatingStats = RatingStats {rating :: Rating, numUsers :: NumUsers}

instance Show RatingStats where
  show (RatingStats (Rating rating) (NumUsers numUsers)) =
    (show rating) ++ "@" ++ (show numUsers)

type RatingTable = Map.Map Demographic RatingStats

instance {-# OVERLAPS #-} Show RatingTable where
  show table =
    Map.toList table
    & map show
    & intercalate "\n"

a @. b = a @: [hasClass b]

tableToMap :: [[RatingStats]] -> RatingTable
tableToMap table =
  Map.fromList $ zip [(age, gender) | gender <- enumerate :: [Gender], age <- enumerate :: [Age]] (mconcat table)

-- TODO: operator (//) implies arbitrarly deep nesting, i need direct nesting (children)

scrapeRatingTable :: Scraper Text.Text RatingTable
scrapeRatingTable =
  chroots ("td" @. "ratingTable") cell
  & chroots "tr"
  & fmap (tableToMap . tail) -- skip the first row, it's just headings (it will be an empty list)
  & chroots ("div" @: ["class" @= "title-ratings-sub-page"] // "table")
  & fmap (\(_:x:_:[]) -> x) -- the second table is the relevant one
  -- TODO: ^^ treba bacit neki maybe, nekako u monad strpat činjenicu da nekad ne prolazi
  where
    cell :: Scraper Text.Text RatingStats
    cell = do
      rating <- text $ "div" @. "bigcell"
      numUsers <- text $ "div" @. "smallcell" // "a"

      -- TODO: maybe a monad transformer for this?
      case (Text.Read.rational rating, Text.Read.decimal $ Text.filter isDigit numUsers) of
        (Right (parsedRating, _), Right (parsedNumUsers, _)) ->
          return $ RatingStats (Rating parsedRating) (NumUsers parsedNumUsers)
        _ ->
          fail "can't parse cell"

scrapeMoviesFromListPage :: Scraper Text.Text (Vector URL.URL)
scrapeMoviesFromListPage =
  ("div" @. "lister-list" // "h3" @. "lister-item-header" // "a")
  & attrs "href"
  & fmap (Vector.fromList . catMaybes . fmap (URL.importURL . Text.unpack))

scrapeWholeList :: String -> IO (Vector URL.URL)
scrapeWholeList url =
  rec url 1 Vector.empty
  where
    rec :: String -> Int -> Vector URL.URL -> IO (Vector URL.URL)
    rec url page collected = do
      Just links <- scrapeURL (url ++ "?sort=list_order,asc&st_dt=&mode=detail&page=" ++ show page) scrapeMoviesFromListPage
      if Vector.null links then -- stop iterating when scraping fails (on 404)
        return collected
      else
        rec url (page + 1) (collected <> links)

-- TODO: generalize over list, traversable?
mapWhile :: (Monad m) => (a -> m b) -> (b -> Bool) -> [a] -> m [b]
mapWhile f stopPredicate (x:xs) = do
  y <- f x
  if stopPredicate y then 
    return []
  else do
    ys <- mapWhile f stopPredicate xs
    return $ y:ys
mapWhile f _ [] =
  return []


data Movie = Movie { movieURL :: URL.URL, ratingTable :: RatingTable } deriving (Show)

moviePageToRatingsPage :: URL.URL -> URL.URL
moviePageToRatingsPage (URL.URL url_type url_path url_params) =
  URL.URL url_type (url_path ++ "ratings") []

addImdbHost :: URL.URL -> URL.URL
addImdbHost url =
  url {URL.url_type = URL.Absolute $ URL.Host (URL.HTTP True) "www.imdb.com" Nothing}

catMaybesT :: (Traversable t, Monoid (t a)) => t (Maybe a) -> t a
catMaybesT t =
  fromMaybe mempty $ traverse id t

-- is the quantifier the only way to get this to typecheck?
getAllRatings ::  (Traversable t, forall a. Monoid (t a)) => t URL.URL -> IO (t Movie)
getAllRatings urls =
  catMaybesT <$> mapM f urls
  where
    f :: URL.URL -> IO (Maybe Movie)
    f url =
      scrapeURL (URL.exportURL ratingsURL) scrapeRatingTable
      & fmap (fmap $ Movie ratingsURL)
      where
        ratingsURL =
          addImdbHost $ moviePageToRatingsPage url


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
    

testGetAllRatings :: IO ()
testGetAllRatings = do
  links <- scrapeWholeList "https://www.imdb.com/list/ls023670262/"
  movies <- getAllRatings links
  putStrLn $ [i|#{length movies}/19|]
  print $ Vector.take 5 movies

testTable :: IO ()
testTable = do
  Just h <- scrapeURL "https://www.imdb.com/title/tt0111161/ratings?ref_=tt_ov_rt" scrapeRatingTable
  putStrLn $ show h

testListPage :: IO ()
testListPage = do
  Just h <- scrapeURL "https://www.imdb.com/list/ls057823sad54/?sort=list_order,asc&st_dt=&mode=detail&page=2" scrapeMoviesFromListPage
  -- mapM_ (\(MovieURL url) -> putStrLn url) h
  return ()

main :: IO ()
main = return ()
