{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Router (Route,router,dir) where

import Data.Text (Text,splitOn)
import Network.Wai (Request(..),Application)

-- | Alias for a function which maps path pieces to applications.
type Route = ([Text] -> Maybe Application)


-- | Router for mapping paths to applications.
-- 
-- For example:
-- 
-- > router [ dir "/foo" fooApp
-- >        , dir "/api" apiApp 
-- >        ] defaultApp
router :: [Route] -> Application -> Application
router routes d req = case router' (pathInfo req) routes of
  Nothing -> d req
  Just  a -> a req


-- | First matching paths' application, nothing otherwise.
router' :: [Text] -> [Route] -> Maybe Application
router' _  []     = Nothing
router' ps (r:rs) = case r ps of
  Nothing -> router' ps rs
  Just  a -> Just a
  

-- | A possible web application if the path matches, nothing otherwise.
dir :: Text -> Application -> Route
dir path a = let ps = pathPieces path
             in  ( \xs -> if ps == xs 
                          then Just  a
                          else Nothing )


-- | Pieces of a URL path.
pathPieces :: Text -> [Text]
pathPieces path = filter (""/=) $ splitOn "/" path
