import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)

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

  p $ do
    "Some rational functions cannot be symbolically integrated yet. "
    "Some may not be feasible to derive if they require solutions to polynomials of degree 5 or more. "
    "Others would be feasible, but would require support for deriving real roots of "
    "simultaneous polynomials beyond those that are rational numbers, "
    "or support for solving general quartic polynomials. "

  integralExamples
    [ 1 / (1 + x ** 4),
      x / (1 + x ** 10),
      (x ** 5 + 4 * x ** 2) / (x ** 3 + 2 * x ** 2 + 3)
    ]
