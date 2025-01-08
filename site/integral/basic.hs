import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p "These are examples of basic integrals integrated by Symtegration."

  integralExamples
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
