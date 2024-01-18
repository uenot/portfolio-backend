{-# LANGUAGE OverloadedStrings #-}

module Server where

import Web.Scotty
import Repos.Fetch
import Data.Aeson

startServer :: IO ()
startServer = scotty 3000 routes

routes :: ScottyM ()
routes = do
    get "/test" testRoute
    get "/foo" $ do
        text "bar"

testRoute :: ActionM ()
testRoute = do
    repos <- liftIO fetchRepos
    json repos

instance ToJSON Repo where
    toJSON (Repo name url time _ (Just langs) (Just commits) (Just contribs)) =
        object ["name" .= name,
                "url" .= url,
                "time" .= time,
                "languages" .= langs,
                "commits" .= commits,
                "contributors" .= contribs]

instance ToJSON TimeDetails where
    toJSON (TimeDetails created updated) =
        object ["createdAt" .= created,
                "updatedAt" .= updated]

instance ToJSON Languages where
    toJSON (Languages langs) = toJSON langs

instance ToJSON Commit where
    toJSON (Commit time msg) = 
        object ["time" .= time,
                "commitMessage" .= msg]

instance ToJSON Contributor where
    toJSON (Contributor name url ct) =
        object ["name" .= name,
                "profileUrl" .= url,
                "contributions" .= ct]