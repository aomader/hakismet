import Network.Browser
import Network.HTTP
import Network.URI

{- | 
    verifyKey simply tries to verify your API key, it should be called before every
    other akismet related operation.
-}
verifyKey :: String -- ^ The Akismet API key
          -> String -- ^ The blog url
          -> IO Bool
verifyKey key blog = do
    response <- simpleHTTP $ formToRequest (Form POST uri [("key", key), ("blog", blog)])
    body <- getResponseBody response
    return (body == "valid")
  where
    Just uri = parseURI "http://rest.akismet.com/1.1/verify-key"
