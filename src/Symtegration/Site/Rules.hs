-- |
-- Description: Rules for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Rules where

import Hakyll

-- | Hakyll rules to generate the web site.
rules :: Rules ()
rules = do
  match "templates/*" $ compile templateBodyCompiler

  match "images/**.png" $ do
    route idRoute
    compile copyFileCompiler
