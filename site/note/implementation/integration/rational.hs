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
    "See also "
    a ! href "/note/implementation/" $ "more implementation notes on Symtegration"
    "."

  section ! A.id "coefficients" $ do
    h2 "Rational number coefficients"

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

    p $ do
      "If for a rational function \\(h = \\frac{A}{D}\\), the degree of \\(A\\) is less than the degree of \\(D\\) "
      "and \\(D\\) is squarefree, then there exist algorithms which can integrate \\(h\\). "
      "A squarefree polynomial is a polynomial which has no square non-constant factor, "
      "i.e., there is no non-constant polynomial \\(B\\) such that \\(B^2\\) divides "
      "the polynomial exactly.  The particular algorithm which Symtegration uses for "
      "this purpose is the Lazard-Rioboo-Trager algorithm. "

    p $ do
      "Specifically, the Lazard-Rioboo-Trager algorithm finds pairs of polynomials \\((Q_i(t), S_i(t, x))\\) such that "

    displayMath "\\int \\frac{A}{D} \\, dx = \\sum_{i=1}^n \\sum_{\\alpha \\mid Q_i(\\alpha) = 0} \\alpha \\log S_i(\\alpha, x)"

    p $ do
      "Note that each \\(S_i\\) is a polynomial of two variables. "
      "If we can find all roots for all \\(Q_i\\), we can fully write out the integral. "
      "However, the roots can be complex numbers, so the integral may involve complex logarithms. "
      "For example,"

    displayMath $
      "\\int \\frac{x^4-3x^2+6}{x^6-5x^4+5x^2+4} \\, dx = "
        <> "\\sum_{\\alpha \\mid \\alpha^2+\\frac{1}{4}=0} \\alpha \\log \\left( (800\\alpha^3 - 14\\alpha)x^3 - (400\\alpha^2-7)x^2 - (2440\\alpha^3-32\\alpha)x +792\\alpha^2 - 16 \\right)"

  section ! A.id "complextoatan" $ do
    h2 "Switching from complex logarithms to inverse tangents"

    p $ do
      "With integrals using complex logarithms, one problem is that definite integrals "
      "cannot be computed in Haskell with such logarithms when evaluated using non-complex "
      "number types. "

    p $ do
      "A more serious problem is that there can be discontinuities in the "
      "indefinite integral despite the integrand and interval being continuous. "
      "For example, the integral in the "
      a ! href "#logterms" $ "previous section"
      " has a discontinuity at \\(\\pm\\sqrt{2}\\).  If we tried to compute the definite integral "
      "from 1 to 2 by substituting \\(x=1\\) and \\(x=2\\), we would get about -3.46, "
      "when the correct value is about 2.81. "

    p $ do
      "This problem can be avoided if we can convert the complex logarithms into continuous real functions. "
      "We use part of Rioboo's algorithm to turn complex logarithms into sums of inverse tangents. "
      "Given polynomials \\(A\\) and \\(B\\) with real number coefficients, we can derive a sum of "
      "tangents \\(f\\) such that "

    displayMath "\\frac{df}{dx} = \\frac{d}{dx} \\left( i \\log \\frac{A + iB}{A - iB} \\right)"

    p "This provides a method to convert complex logarithms to real functions.  For example, "

    displayMath $
      "\\frac{d}{dx} \\left( i \\log \\frac{(x^3-3x)+i(x^2-2)}{(x^3-3x)-i(x^2-2)} \\right) = "
        <> "\\frac{d}{dx} \\left( 2 \\tan^{-1} \\frac{x^5-3x^2+x}{2} + 2 \\tan^{-1} x^3 + 2 \\tan^{-1} x \\right)"

    p $ do
      "If we can turn a complex logarithm with any polynomial argument into the form \\(i\\log\\frac{A+iB}{A-iB}\\), "
      "this provides a method to turn the complex logartihm into a real function.  This is described in the "
      a ! href "#complextoreal" $ "next section"
      "."

  section ! A.id "complextoreal" $ do
    h2 "Switching from complex logarithms to real functions"

    p $ do
      strong "Warning: "
      em "Section not written yet."

  section ! A.id "solvepolynomials" $ do
    h2 "Solving polynomials"

    p $ do
      strong "Warning: "
      em "Section not written yet."

  section ! A.id "together" $ do
    h2 "Tying it all together"

    p $ do
      strong "Warning: "
      em "Section not written yet."
