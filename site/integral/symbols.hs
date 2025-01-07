import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Symtegration
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p "These are examples of integrals with symbols other than the variable integrated by Symtegration."

  mapM_ showExpr expressions

showExpr :: Expression -> Html
showExpr e = p $ toHtml equation
  where
    equation = "\\( \\int " <> toLaTeX e <> " \\, dx = " <> t <> " \\)"
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
    exp "a" * sin "x",
    x * log ("a" * x ** 2),
    log ("a" + "x") + log ("b" * "x")
  ]
