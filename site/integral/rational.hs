module Main where

import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Symtegration
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p $ do
    "These are examples of rational functions integrated by Symtegration.  "
    "Here, rational functions mean the ratio of two polyomials, not functions of rational numbers.  "

  table ! class_ "integrals" $ do
    thead $ tr $ th "Haskell" >> th "\\(f\\)" >> th "\\(\\int f \\, dx\\)"
    tbody $ mapM_ showExpr expressions

  section $ do
    h2 "Unsupported integrals" ! A.id "unsupported"
    p $ do
      "Some rational functions cannot be symbolically integrated yet. "
      "Some may not be feasible to derive if they require solutions to polynomials of degree 5 or more. "
      "Others would be feasible, but would require support for deriving real roots of "
      "simultaneous polynomials beyond those that are rational numbers, "
      "or support for solving general cubic and quartic polynomials. "

  table ! class_ "integrals" $ do
    thead $ tr $ th "Haskell" >> th "\\(f\\)" >> th "\\(\\int f \\, dx\\)"
    tbody $
      mapM_
        showExpr
        [ 1 / (1 + x ** 4),
          x / (1 + x ** 10),
          (x ** 5 + 4 * x ** 2) / (x ** 3 + 2 * x ** 2 + 3)
        ]

showExpr :: Expression -> Html
showExpr e = tr $ do
  td $ code $ toHtml $ toHaskell e
  td $ toHtml $ "\\(" <> toLaTeX e <> "\\)"
  td $ toHtml $ "\\(" <> t <> "\\)"
  where
    t
      | Just e' <- integrate "x" e = toLaTeX e'
      | otherwise = "\\bot"

-- | Use this as the variable for all the integrals.
x :: Expression
x = "x"

expressions :: [Expression]
expressions =
  [ x / (1 + x),
    x ** 2 / (1 + x ** 2),
    (x ** 2 + x) / (x - 1),
    1 / (x ** 3 - x ** 5),
    (x ** 6 + x + 1) / (x ** 2 - 1),
    (x ** 4 - 3 * x ** 2 + 6) / (x ** 6 - 5 * x ** 4 + 5 * x ** 2 + 4)
  ]
