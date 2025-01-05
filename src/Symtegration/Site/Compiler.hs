-- |
-- Description: Hakyll compilers for generating the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Compiler where

import Data.ByteString.Lazy (ByteString)
import Hakyll

-- |
-- Run the Haskell code in the underlying file and use its output.
-- This can compile both Haskell code and literate Haskell code.
haskellCompiler ::
  -- | Extra flags to pass to @runhaskell@.
  [String] ->
  Compiler (Item ByteString)
haskellCompiler args = do
  file <- getResourceFilePath
  emptyItem >>= withItemBody (run file)
  where
    -- Run the Haskell code in the given file and return its standard output.
    run f = unixFilterLBS "runhaskell" $ concat [defaultArgs, args, [f]]

    -- Default flags to always use with @runhaskell@.
    defaultArgs = ["-XGHC2021", "-XOverloadedStrings", "-XLambdaCase"]

    -- We will run the code from the file directly,
    -- so we don't care about any content in an item.
    emptyItem = makeItem ""