import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes as A hiding (cite)

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p $ do
    "This note explains how Symtegration integrates rational functions, which is done by the "
    symtegrationModule ["Integration", "Rational"]
    " module.  Here, rational functions mean the ratio of two polynomials, not functions of rational numbers. "
    "The implementation is based on "
    cite $ a ! href "https://link.springer.com/book/10.1007/b138171" $ "Symbolic Integration I: Transcendental Functions"
    " by Manuel Bronstein."

  p $ do
    "Symtegration supports the integration of rational functions with only rational number coefficients. "
    "It will not know what to do if the coefficients are symbolic. "
    "This does not necessarily mean that any symbol appearing in a rational function will "
    "prevent its integration.  For example, if the symbolic coefficient is a constant factor "
    "on the entire rational function, we can integrate the rational function even if there "
    "is a non-variable symbol. "

  p "For instance, Symtegration will not integrate the following, "

  displayMath $ integralExample $ x ** 2 / ("a" + "a" * x ** 2)

  p "but it will integrate the following. "

  displayMath $ integralExample $ (1 / "a") * (x ** 2 / (1 + x ** 2))

  p $ do
    "See also "
    a ! href "/note/implementation/" $ "more implementation notes on Symtegration"
    "."

  section ! A.id "representation" $ do
    h2 "Representing rational functions"

    p $ do
      "While the symbolic representation of mathematical expressions in Symtegration faithfully "
      "represents the semantics of mathematical expressions written in Haskell, "
      "we also use a special representation for polynomials to support the many "
      "algorithsm for polynomials that are useful for symbolic integration. "
      "The "
      symtegrationModule ["Polynomial"]
      " module includes the general operations that our polynomial representations need to support "
      "and various algorithms for polynomials such as finding the greatest common divisor.  The "
      symtegrationModule ["Polynomial", "Indexed"]
      " module defines a concrete representation for polynomials. "

  section ! A.id "hermite" $ do
    h2 "Hermite reduction"

    p $ do
      "For polynomials \\(A\\) and \\(D\\) where the greatest common divisor is a constant, "
      "Hermite reduction finds rational functions \\(g\\) and \\(h\\) such that "
      "\\(h\\) has a squarefree denominator and "

    displayMath "\\frac{A}{D} = \\frac{dg}{dx} + h"

    p "This implies "

    displayMath "\\int \\frac{A}{D} \\, dx = g + \\int h \\, dx"

    p $ do
      "With algortithms to integrate rational functions with squarefree denominators, "
      "we can then derive the integral for \\(\\frac{A}{D}\\). "

    p "For example, with Hermite reduction we can infer that "

    displayMath $
      "\\int \\frac{x^7 - 24x^4 - 4x^2 + 8x - 8}{x^8 + 6x^6 + 12x^4 + 8x^2} \\, dx = "
        <> "\\frac{3}{x^2+2} + \\frac{8x^2+4}{x^5+4x^3+4x} + \\int \\frac{1}{x} \\, dx"

  section ! A.id "logterms" $ do
    h2 "Integrating the logarithmic terms"

  section ! A.id "complextoatan" $ do
    h2 "Switching from complex logarithms to inverse tangents"

  section ! A.id "complextoreal" $ do
    h2 "Switching from complex logarithms to real functions"

  section ! A.id "together" $ do
    h2 "Tying it all together"
