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
import Text.Pandoc.Highlighting (pygments, styleToCss, zenburn)

-- | Hakyll rules to generate the web site.
rules :: Rules ()
rules = do
  match "index.markdown" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "template/default.html" siteContext

  match "integral/**" $ do
    route toIndex
    compile $
      haskellCompiler []
        >>= pure . fmap unpack
        >>= loadAndApplyTemplate "template/default.html" siteContext

  match ("style/**.hs" .||. "style/**.lhs") $ do
    route $ setExtension "css"
    compile $ haskellCompiler []

  -- Syntax highlighting in light mode.
  create ["style/syntax-light.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pygments

  -- Syntax highlighting in dark mode.
  create ["style/syntax-dark.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss zenburn

  match "image/**.png" $ do
    route idRoute
    compile copyFileCompiler

  match "LICENSE.txt" $ do
    route idRoute
    compile copyFileCompiler

  match "template/**" $ do
    compile templateBodyCompiler
