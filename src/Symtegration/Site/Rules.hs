-- |
-- Description: Hakyll rules for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Rules where

import Hakyll
import Symtegration.Site.Context

-- | Hakyll rules to generate the web site.
rules :: Rules ()
rules = do
  match "template/**" $ do
    compile templateBodyCompiler

  match "index.markdown" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "template/default.html" siteContext

  match "image/**.png" $ do
    route idRoute
    compile copyFileCompiler

  match "LICENSE" $ do
    route idRoute
    compile copyFileCompiler
