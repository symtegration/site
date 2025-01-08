-- |
-- Description: Helper functions for generating content on the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Content where

import Symtegration
import Text.Blaze.Html5

-- | Turn a list of expressions into HTML showing an expression integrated into its integral.
-- Each equation is in a paragraph.
integralExamples :: [Expression] -> Html
integralExamples = mapM_ (p . integralExample)

-- | Turn an expression into HTML showing an expression integrated into its integral.
-- Expressions which cannot be integrated will have integrals shown as the bottom symbol.
integralExample :: Expression -> Html
integralExample e = toHtml equation
  where
    equation = "\\( \\int " <> toLaTeX e <> " \\, dx = " <> t <> " \\)"
    t
      | Just e' <- integrate "x" e = toLaTeX e'
      | otherwise = "\\bot"

-- | Use this as the variable for example integrals with 'integralExample' and 'integralExamples'.
x :: Expression
x = "x"
