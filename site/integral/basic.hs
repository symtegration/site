import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Symtegration
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p "These are examples of basic integrals integrated by Symtegration."

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
  [ 1,
    x,
    x ** 2,
    sqrt x,
    1 / x,
    exp x,
    log x,
    sin x,
    tan x
  ]
