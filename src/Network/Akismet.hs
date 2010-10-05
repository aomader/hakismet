module Network.Akismet
    (
      -- * Akismet API
      verifyKey
    , checkComment
    , submitSpam
    , submitHam

      -- * Data types
    , Comment (..)
    , defaultComment
    ) where

import Network.Browser
import Network.HTTP
import Network.URI

-- | Comment represents the Content you want to check using Akismet.
-- For the exact meaning of each record selector check
-- http://akismet.com/development/api/
data Comment = Comment
    { cBlog :: String
    , cUserIp :: String
    , cUserAgent :: String
    , cContent :: String
    , cReferrer :: Maybe String
    , cPermalink :: Maybe String
    , cType :: Maybe String
    , cAuthor :: Maybe String
    , cAuthorEmail :: Maybe String
    , cAuthorUrl :: Maybe String
    , cEnvVars :: [(String, String)]
    }

userAgent :: String
userAgent = "HAkismet/0.1"

-- | Create a Comment with all required fields
defaultComment :: String        -- ^ Blog
               -> String        -- ^ UserIp
               -> String        -- ^ UserAgent
               -> String        -- ^ Content
               -> Comment
defaultComment blog userip useragent content =
    Comment { cBlog = blog
            , cUserIp = userip
            , cContent = content
            , cUserAgent = useragent
            , cReferrer = Nothing
            , cPermalink = Nothing
            , cType = Nothing
            , cAuthor = Nothing
            , cAuthorEmail = Nothing
            , cAuthorUrl = Nothing
            , cEnvVars = []
            }

-- | Try to verify your API key, it should be called before
-- every other akismet related operation.
verifyKey :: String         -- ^ Akismet API key
          -> String         -- ^ Blog url
          -> IO Bool
verifyKey key blog = do
    response <- simpleHTTP $ formToRequest (Form POST uri [("key", key), ("blog", blog)])
    either (error . ("verifyKey: " ++) . show)
           (return . ("valid" ==) . rspBody)
           response
  where
    Just uri = parseURI "http://rest.akismet.com/1.1/verify-key"

-- | Check a comment, in case of spam it returns True else False
checkComment :: String  -- ^ Akismet API key
             -> Comment -- ^ Comment
             -> IO Bool
checkComment key comment = do
    response <- simpleHTTP $ createRequest key "comment-check" comment
    either (error . ("checkComment: " ++) . show)
           (return . ("true" ==) . rspBody)
           response

-- | Submit a spam comment
submitSpam :: String  -- ^ Akismet API key
           -> Comment -- ^ Spam comment
           -> IO ()
submitSpam key comment = do
    _ <- simpleHTTP $ createRequest key "submit-spam" comment
    return ()

-- | Submit a false positive spam comment aka ham
submitHam :: String  -- ^ Akismet API key
          -> Comment -- ^ Ham comment (false positive)
          -> IO ()
submitHam key comment = do
    _ <- simpleHTTP $ createRequest key "submit-ham" comment
    return ()

createRequest :: String
              -> String
              -> Comment
              -> Request_String
createRequest key service comment = formToRequest $ Form POST uri values
  where
    values = [ ("blog", cBlog comment)
             , ("user_ip", cUserIp comment)
             , ("user_agent", cUserAgent comment)
             , ("comment_content", cContent comment)
             ] ++ (cEnvVars comment)
    uri = case parseURI ("http://" ++ key ++ ".rest.akismet.com/1.1/" ++ service) of
               Nothing -> error "createRequest: unable to create URI"
               Just s  -> s
