-- |
-- Description: Custom Hakyll context for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Context where

import Hakyll
import System.FilePath (takeBaseName)

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

-- | Custom context used when including examples of integrals in the main example page.
integralExampleContext :: Context String
integralExampleContext = field "anchor" anchor <> bodyContext <> siteContext
  where
    anchor item = pure $ takeBaseName $ toFilePath $ itemIdentifier item
    bodyContext = mapContext demoteHeaders $ bodyField "body"

-- | Custom context for notes.
-- In particular, it sets @custombody@ signifying that the default template should
-- delegate everything that goes into @body@ to the resource body.
-- It can also include a list of authors encoded like @"name1|url1,name2,name3|url3"@.
noteContext :: Context String
noteContext = boolField "custombody" (const True) <> authorField <> siteContext
  where
    authorField = listFieldWith "author" siteContext linkAuthors

    linkAuthors item = do
      value <- getMetadataField (itemIdentifier item) "author"
      case value of
        Nothing -> noResult "no authors"
        Just s -> pure $ map (flip itemSetBody item . encode) $ decode s

    decode s = map split $ splitAll "," s

    split s
      | [name] <- l = (name, Nothing)
      | (name : url : _) <- l = (name, Just url)
      | otherwise = ("", Nothing)
      where
        l = splitAll "\\|" s

    encode (name, Nothing) = authorSpan $ nameSpan name
    encode (name, Just url) = authorSpan $ link url $ nameSpan name

    authorSpan t = "<span itemprop='author' itemscope itemtype='https://schema.org/Person'>" <> t <> "</span>"
    nameSpan t = "<span itemprop='name'>" <> t <> "</span>"
    link url t = "<a href='" <> url <> "' itemprop='url'>" <> t <> "</a>"
