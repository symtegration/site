-- |
-- Description: Custom Hakyll context for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Context where

import Hakyll

-- | Custom context to use instead of 'defaultContext'.
siteContext :: Context String
siteContext = field "url" clean <> defaultContext
  where
    -- Clean up "index.html" from URLs.
    clean item = do
      path <- getRoute (itemIdentifier item)
      case path of
        Nothing -> noResult "no route for identifier"
        Just s -> pure . cleanupIndexUrl . toUrl $ s

-- | Strips @index.html@ from local URLs.
cleanupIndexUrl :: String -> String
cleanupIndexUrl url@('/' : _) -- only clean up local URLs
  | Nothing <- prefix = url -- does not end with index.html
  | Just s <- prefix = s -- clean up index.html from URL
  where
    prefix = needlePrefix "index.html" url
cleanupIndexUrl url = url
