{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Web.Scotty
import Data.Aeson
import Network.HTTP.Simple
import Data.Aeson.Types (Parser)
import Data.Time
import Data.List (nub)
import Data.Aeson.KeyMap (foldrWithKey, KeyMap)
import Data.Monoid
import Data.Maybe (mapMaybe)

type GithubResponse = [GithubEvent]
data GithubEvent = GithubEvent {
    payload :: GithubPayload,
    time :: UTCTime,
    repo :: GithubRepo
  } deriving (Show)

data GithubPayload =
    PushPayload { pushID :: Int, numCommits :: Int }
  | CreatePayload { refType :: String }
  | UnknownPayload
  deriving (Show)

data GithubRepo = GithubRepo {
  repoID :: Int,
  owner :: String,
  repoName :: String,
  url :: String,
  langs :: Maybe GithubLanguages
} deriving (Show, Eq)

data GithubLanguage = GithubLanguage {
  langName :: String,
  langBytes :: Int
} deriving (Show, Eq)

newtype GithubLanguages = GithubLanguages { getLanguages :: [GithubLanguage]} deriving (Show, Eq)

helper :: Key -> Value -> Parser GithubLanguages -> Parser GithubLanguages
helper k v acc = do
  v' <- parseJSON v
  acc' <- acc
  let langs = getLanguages acc'
  return $ GithubLanguages $ GithubLanguage (show k) v' : langs

instance FromJSON GithubLanguages where
  parseJSON (Object o) = foldrWithKey helper (return (GithubLanguages [])) o


instance FromJSON GithubRepo where
  parseJSON :: Value -> Parser GithubRepo
  parseJSON (Object v) = do
    id <- v .: "id"
    name <- v .: "name"
    url <- v .: "url"
    let (owner, name') = break (=='/') name
    return $ GithubRepo id owner (tail name') url Nothing

parsePayload :: String -> Value -> Parser GithubPayload
parsePayload "PushEvent" (Object v) = do
  pushID <- v .: "push_id"
  numCommits <- v .: "size"
  return $ PushPayload pushID numCommits
parsePayload "CreateEvent" (Object v) = do
  refType <- v .: "ref_type"
  return $ CreatePayload refType
parsePayload _ _ = return UnknownPayload

instance FromJSON GithubEvent where
  parseJSON (Object v) = do
    tag <- v .: "type"
    payload <- v .: "payload"
    payload' <- parsePayload tag payload
    time <- v .: "created_at"
    repo <- v .: "repo"
    return $ GithubEvent payload' time repo

extractRepos :: [GithubEvent] -> [GithubRepo]
extractRepos = nub . map repo

getRepoLanguage :: GithubRepo -> IO GithubRepo
getRepoLanguage r = do
  req <- parseRequest $ url r ++ "/languages"
  resp <- httpJSON $ addRequestHeader "User-Agent" "u/uenot" req
  let body = getResponseBody resp :: GithubLanguages
  let r' = r { langs = Just body}
  return r'

augmentRepos :: [GithubRepo] -> IO [GithubRepo]
augmentRepos = mapM getRepoLanguage

data LanguageFrequencyData = LanguageFrequencyData {
  lang :: String,
  bytes :: Int,
  frequency :: Int,
  propByRepo :: Double,
  propByBytes :: Double
}

getAllLanguages :: [GithubRepo] -> GithubLanguages
getAllLanguages = GithubLanguages . concatMap getLanguages . mapMaybe langs

getLanguagesOverallStats :: GithubLanguages -> (Int, Int)
getLanguagesOverallStats ls = let len = length . nub  $ getLanguages ls in
  let totalBytes = foldMap (Sum . langBytes) $ getLanguages ls in
    (len, getSum totalBytes)

main = do
  response <- httpJSON $ addRequestHeader "User-Agent" "u/uenot" "https://api.github.com/users/uenot/events"
  newRepos <- augmentRepos $ extractRepos (getResponseBody response :: GithubResponse)
  let stats = getLanguagesOverallStats $ getAllLanguages newRepos
  print $ getAllLanguages newRepos

