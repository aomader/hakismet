module Network.Akismet
    ( verifyKey
    ) where

import Network.Browser
import Network.HTTP
import Network.URI

-- | verifyKey simply tries to verify your API key, it should be called before
-- every other akismet related operation.
verifyKey :: String         -- ^ The Akismet API key
          -> String         -- ^ The blog url
          -> IO Bool
verifyKey key blog = do
    response <- simpleHTTP $ formToRequest (Form POST uri [("key", key), ("blog", blog)])
    either (error . ("verifyKey: " ++) . show)
           (return . ("valid" ==) . rspBody)
           response
  where
    Just uri = parseURI "http://rest.akismet.com/1.1/verify-key"
