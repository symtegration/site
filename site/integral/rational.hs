import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p $ do
    "These are examples of rational functions integrated by Symtegration.  "
    "Here, rational functions mean the ratio of two polyomials, not functions of rational numbers.  "

  integralExamples
    [ x / (1 + x),
      x ** 2 / (1 + x ** 2),
      (x ** 2 + x) / (x - 1),
      1 / (x ** 3 - x ** 5),
      (x ** 6 + x + 1) / (x ** 2 - 1),
      (x ** 4 - 3 * x ** 2 + 6) / (x ** 6 - 5 * x ** 4 + 5 * x ** 2 + 4)
    ]

  section ! A.id "complex" $ do
    h2 "With complex logarithms"

    p $ do
      "Symtegration makes an effort to return integrals in terms of purely real functions if it can, "
      "but if it cannot, it will try to return integrals which use complex logarithms. "
      "Care should be taken when using indefinite integrals with complex logarithms "
      "to compute definite integrals, since they may have discontinuities within the integration interval. "

    integralExamples
      [ 1 / (1 + x ** 3),
        x / (1 + x ** 3),
        (x ** 2 - x + 5) / (x ** 4 - 2 * x ** 3 + 5 * x ** 2 - 4 * x + 4)
      ]

  section ! A.id "unsupported" $ do
    h2 "Unsupported integrals"

    p $ do
      "Some rational functions cannot be symbolically integrated yet. "
      "Some may not be feasible to derive if they require solutions to polynomials of degree 5 or more. "

    integralExamples
      [ 1 / (1 + x ** 5),
        x / (1 + x ** 10)
      ]
