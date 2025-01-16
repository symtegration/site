-- |
-- Description: Hakyll rules for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Rules where

import Data.ByteString.Lazy.Char8 (unpack)
import Hakyll
import Symtegration.Site.Compiler
import Symtegration.Site.Context
import Symtegration.Site.Route

-- | Hakyll rules to generate the web site.
rules :: Rules ()
rules = do
  match "index.markdown" $ do
    route $ setExtension "html"
    compile $
      docCompiler
        >>= saveSnapshot "sitemap"
        >>= loadAndApplyTemplate "template/default.html" siteContext

  match "integral/**.hs" $ do
    route toIndex
    compile $
      haskellCompiler []
        >>= pure . fmap unpack
        >>= saveSnapshot "sitemap"
        >>= saveSnapshot "example"
        >>= loadAndApplyTemplate "template/default.html" siteContext

  create ["integral/index.html"] $ do
    route idRoute
    compile $ do
      examples <- chronological =<< loadAllSnapshots "integral/**.hs" "example"
      let exampleContext =
            mconcat
              [ constField "title" "Integration examples",
                constField "description" "Example of integrals supported by Symtegration.",
                listField "examples" integralExampleContext $ pure examples,
                siteContext
              ]
      makeItem ""
        >>= saveSnapshot "sitemap"
        >>= loadAndApplyTemplate "template/examples.html" exampleContext
        >>= loadAndApplyTemplate "template/default.html" exampleContext

  match "note/**.markdown" $ do
    route toIndex
    compile $
      docCompiler
        >>= saveSnapshot "sitemap"
        >>= loadAndApplyTemplate "template/note.html" noteContext
        >>= loadAndApplyTemplate "template/default.html" noteContext

  match "note/**.hs" $ do
    route toIndex
    compile $
      haskellCompiler []
        >>= pure . fmap unpack
        >>= saveSnapshot "sitemap"
        >>= loadAndApplyTemplate "template/note.html" noteContext
        >>= loadAndApplyTemplate "template/default.html" noteContext

  match ("**.markdown" .&&. complement "index.markdown") $ do
    route toIndex
    compile $
      docCompiler
        >>= saveSnapshot "sitemap"
        >>= loadAndApplyTemplate "template/default.html" siteContext

  match ("**.hs" .&&. complement "integral/**.hs") $ do
    route toIndex
    compile $
      haskellCompiler []
        >>= pure . fmap unpack
        >>= saveSnapshot "sitemap"
        >>= loadAndApplyTemplate "template/default.html" siteContext

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      let patterns = "**.markdown" .||. "**.hs" .||. "integral/index.html"
      urls <- loadAllSnapshots patterns "sitemap"
      let sitemapContext = listField "urls" siteContext (pure urls) <> siteContext
      makeItem ""
        >>= loadAndApplyTemplate "template/sitemap.xml" sitemapContext

  match "404.html" $ do
    route idRoute
    compile $ do
      docCompiler
        >>= loadAndApplyTemplate "template/default.html" siteContext

  match "style/**.lhs" $ do
    route $ setExtension "css"
    compile $ haskellCompiler []

  match ("image/**.png" .||. "image/**.webp") $ do
    route idRoute
    compile copyFileCompiler

  match "robots.txt" $ do
    route idRoute
    compile copyFileCompiler

  match "template/**" $ do
    compile templateBodyCompiler
