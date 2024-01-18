module GithubRequests (createRequest) where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import qualified Data.CaseInsensitive as CI

getToken :: IO String
getToken = do
    loadFile defaultConfig
    getEnv "GITHUB_TOKEN"

createRequest :: String -> IO Request
createRequest url = parseRequest url >>= augmentRequest
    where 
        augmentRequest req = do
            token <- getToken
            return $ (setRequestBearerAuth (pack token)
                . addRequestHeader (CI.mk $ pack "User-Agent") (pack "u/uenot")
                . setRequestQueryString [(pack "per_page", Just  $ pack "100")]) req
