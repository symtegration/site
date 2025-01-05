-- |
-- Description: Program for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Main (main) where

import Hakyll
import Symtegration.Site.Rules

main :: IO ()
main = hakyllWith config rules

config :: Configuration
config =
  defaultConfiguration
    { providerDirectory = "site"
    }
