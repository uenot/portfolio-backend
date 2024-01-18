{-# LANGUAGE OverloadedStrings #-}

module Repos.Fetch where

import Data.Aeson
import Network.HTTP.Simple
import Data.Time
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import GithubRequests
import Control.Monad

data Repo = Repo {
    name :: String,
    url :: String,
    time :: TimeDetails,
    endpts :: AuxEndpoints,
    langs :: Maybe Languages,
    commits :: Maybe [Commit],
    contribs :: Maybe [Contributor]
} deriving (Show)

instance FromJSON Repo where
    parseJSON (Object v) = do
        name <- v .: "name"
        url <- v .: "html_url"
        time <- parseJSON $ Object v
        endpts <- parseJSON $ Object v
        return $ Repo name url time endpts Nothing Nothing Nothing
        -- fix this line above, garbage

data TimeDetails = TimeDetails {
    created :: UTCTime,
    updated :: UTCTime
} deriving (Show)

instance FromJSON TimeDetails where
    parseJSON (Object v) = do
        created <- v .: "created_at"
        updated <- v .: "updated_at"
        return $ TimeDetails created updated

data AuxEndpoints = AuxEndpoints {
    epLangs :: String,
    epCommits :: String,
    epContribs :: String
} deriving (Show)

instance FromJSON AuxEndpoints where
    parseJSON (Object v) = do
        langs <- v .: "languages_url"
        commits <- v .: "commits_url"
        let commits' = takeWhile (/='{') commits
        contribs <- v .: "contributors_url"
        return $ AuxEndpoints langs commits' contribs

newtype Languages = Languages {
    getLanguages :: Map.Map String Int
} deriving (Show)

instance FromJSON Languages where
    parseJSON obj = do
        langs <- parseJSON obj
        return $ Languages langs

data Commit = Commit {
    commitTime :: UTCTime,
    message :: String
} deriving (Show)

instance FromJSON Commit where
    parseJSON (Object v) = do
        mainInfo <- v .: "commit"
        committer <- mainInfo .: "committer"
        time <- committer .: "date"
        message <- mainInfo .: "message"
        return $ Commit time message

data Contributor = Contributor {
    ctName :: String,
    ctUrl :: String,
    ctCount :: Int
} deriving (Show)

instance FromJSON Contributor where
    parseJSON (Object v) = do
        name <- v .: "login"
        url <- v .: "html_url"
        count <- v .: "contributions"
        return $ Contributor name url count

fetchLanguage :: Repo -> IO Repo
fetchLanguage repo = do 
    req <- createRequest $ (epLangs . endpts) repo
    resp <- httpJSON req
    return repo {langs = Just $ getResponseBody resp}

fetchCommit :: Repo -> IO Repo
fetchCommit repo = do
    req <- createRequest $ (epCommits . endpts) repo
    resp <- httpJSON $ addToRequestQueryString [("author", Just "uenot")] req
    return repo {commits = Just $ getResponseBody resp}

fetchContributor :: Repo -> IO Repo
fetchContributor repo = do
    req <- createRequest $ (epContribs . endpts) repo
    resp <- httpJSON req
    return repo {contribs = Just $ getResponseBody resp}

augmentRepo :: Repo -> IO Repo
augmentRepo = fetchLanguage >=> fetchCommit >=> fetchContributor

fetchRepos :: IO [Repo]
fetchRepos = do
    req <- createRequest "https://api.github.com/users/uenot/repos"
    resp <- httpJSON req
    mapM augmentRepo $ getResponseBody resp