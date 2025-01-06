module Main where

import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Symtegration
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p "These are examples of integrals with symbols other than the variable integrated by Symtegration."

  table ! class_ "integrals" $ do
    thead $ tr $ th "Haskell" >> th "\\(f\\)" >> th "\\(\\int f \\, dx\\)"
    tbody $ mapM_ showExpr expressions

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
  [ "a",
    "a" / "b",
    sin ("a" * x),
    x * log ("a" * x ** 2),
    log ("a" + "x") + log ("b" * "x")
  ]
