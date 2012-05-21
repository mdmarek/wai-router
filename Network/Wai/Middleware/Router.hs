{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Router (router,dir) where

import Data.Text (Text,splitOn)
import Network.Wai (Request(..),Application)


-- | Router for mapping paths to applications.
-- 
-- For example:
-- 
-- > router [ dir "/foo" fooApp
-- >        , dir "/api" apiApp 
-- >        ] defaultApp
router :: [([Text] -> Maybe Application)] -> Application -> Application
router routes d req = case router' (pathInfo req) routes of
  Nothing -> d req
  Just  a -> a req


-- | First matching paths' application, nothing otherwise.
router' :: [Text] -> [([Text] -> Maybe Application)] -> Maybe Application
router' _  []     = Nothing
router' ps (r:rs) = case r ps of
  Nothing -> router' ps rs
  Just  a -> Just a
  

-- | A possible web application if the path matches, nothing otherwise.
dir :: Text -> Application -> ([Text] -> Maybe Application)
dir path a = let ps = pathPieces path
             in  ( \xs -> if ps == xs 
                          then Just  a
                          else Nothing )


-- | Pieces of a URL path.
pathPieces :: Text -> [Text]
pathPieces path = filter (""/=) $ splitOn "/" path
                 