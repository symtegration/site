import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p "These are examples of integrals with symbols other than the variable integrated by Symtegration."

  integralExamples
    [ "a",
      "a" / "b",
      "\\mu" * x,
      sin ("a" * x),
      exp "a" * sin "x",
      x * log ("a" * x ** 2),
      log ("a" + "x") + log ("b" * "x")
    ]
