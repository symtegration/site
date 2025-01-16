-- |
-- Description: Helper functions for generating content on the web site.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Site.Content where

import Data.Text (Text)
import Symtegration
import Symtegration.Symbolic
import Text.Blaze.Html5

-- | Include LaTeX as display math.
displayMath :: Text -> Html
displayMath t = toHtml $ "\\[" <> t <> "\\]"

-- | Include LaTeX as inline math.
inlineMath :: Text -> Html
inlineMath t = toHtml $ "\\(" <> t <> "\\)"

-- | Turn an expression into LaTeX showing the unintegrated integral of the expression.
integral :: Expression -> Text
integral e = "\\int " <> delimit (toLaTeX e) <> " \\, d" <> xtext
  where
    delimit v | (_ :+: _) <- e = brace v | (_ :-: _) <- e = brace v | otherwise = v
    brace v = "\\left(" <> v <> "\\right)"

-- | Integrate the given expression and return its LaTeX representation.
-- If integration failed, returns \(\bot\).
integrated :: Expression -> Text
integrated e
  | Just e' <- integrate xtext e = toLaTeX e'
  | otherwise = "\\bot"

-- | Turn a list of expressions into HTML showing an expression integrated into its integral.
-- Each equation is in a paragraph.
integralExamples :: [Expression] -> Html
integralExamples = mapM_ (p . toHtml . inlineMath . integralExample)

-- | Turn an expression into LaTeX showing an expression integrated into its integral.
-- Expressions which cannot be integrated will have integrals shown as the bottom symbol.
integralExample :: Expression -> Text
integralExample e = integral e <> " = " <> integrated e

-- | Use this as the variable for all of the expressions for use with this module.
x :: Expression
x = Symbol xtext

-- | This is the text used for the symbol in 'x'.
xtext :: Text
xtext = "x"
