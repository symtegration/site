-- |
-- Description: Functions related to Hakyll routes for the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Route where

import Hakyll

-- | Route it to @index.html@.
toIndex :: Routes
toIndex = setExtension "/index.html"
