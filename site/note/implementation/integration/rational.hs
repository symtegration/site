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
      "A more serious problem is that there can be non-obvious discontinuities in the "
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
      "If we have a pair of polynomials \\((R(t), S(t,x))\\) from the "
      a ! href "#logterms" $ "Lazard-Rioboo-Trager algorithm"
      " such that "

    displayMath "\\frac{df}{dx} = \\frac{d}{dx} \\left( \\sum_{\\alpha \\mid R(\\alpha)=0} \\alpha \\log S(\\alpha, x) \\right)"

    p $ do
      "where \\(\\alpha\\) denotes the complex number solutions for \\(R(x)=0\\), "
      "then with the other part of Rioboo's algorithm, it is possible to infer "
      "polynomials \\(P(u,v)\\), \\(Q(u,v)\\), \\(A(u,v,x)\\), and \\(B(u,v,x)\\) such that "

    displayMath "\\frac{df}{dx} = \\frac{d}{dx} \\left( \\sum_{\\alpha, \\beta \\mid P(\\alpha, \\beta) = Q(\\alpha, \\beta) = 0, \\beta > 0} \\left( \\alpha \\log \\left( A(\\alpha, \\beta, x)^2 + B(\\alpha, \\beta, x)^2 \\right) + \\beta g_{\\alpha,\\beta}(x) \\right) + \\sum_{\\gamma \\mid R(\\gamma)=0} \\gamma \\log S(\\gamma, x) \\right)"

    p "where"

    ul $ do
      li "\\(u=\\alpha\\) and \\(v=\\beta\\) are real number solutions for \\(P(u,v)=0\\) and \\(Q(u,v)=0\\) such that \\(\\beta > 0\\)"

      li "\\(t=\\gamma\\) are real number solutions for \\(R(t)=0\\)"

      li $ do
        p $ do
          "\\(g_{\\alpha,\\beta}(x)\\) is the real function derived from the "
          "following complex logarithm as specified by the "
          a ! href "#complextoatan" $ "previous section"
          ": "

        displayMath "\\frac{d}{dx} g_{\\alpha,\\beta}(x) = \\frac{d}{dx} \\left( i \\log \\frac{A(\\alpha,\\beta) + i B(\\alpha,\\beta)}{A(\\alpha,\\beta) - i B(\\alpha,\\beta} \\right)"

    p $ do
      "If we can derive all real number solutions for \\(P(u,v)=0\\), \\(Q(u,v)=0\\), and \\(R(t)=0\\), "
      "we could then derive an integral for a rational function in a real function form. "

  section ! A.id "solvepolynomials" $ do
    h2 "Solving polynomials"

    p $ do
      "In the "
      a ! href "#complextoreal" $ "previous section"
      ", we required real number solutions for polynomials to fully integrate "
      "rational functions with real number coefficients in to real functions, "
      "at least if we want a representation concrete enough that we can compute "
      "actual values from. "

    p $ do
      "The "
      symtegrationModule ["Polynomial", "Solve"]
      " module provides some ability to solve univariate polynomials with real number coefficients. "
      "Specifically, it computes all real number roots for linear, quadratic, and cubic polynomials. "
      "It can also compute all real number roots for a number of special cases for quatric polynomials. "

    p $ do
      "We also need to find the real number solutions for two polynomials with two variables. "
      "To do so, we take advantage of the fact that the resultant of two polynomials are zero "
      "if and only if the two polynomials have a common root.  For two polynomials \\(P(u,v)\\) and "
      "\\(Q(u,v)\\), we compute the resultant in terms of \\(u\\), which is a polynomial in \\(v\\). "
      "We find a solution to this polynomial to find values of \\(v\\) where the resultant is zero, "
      "so that there must be a common \\(u\\) which satisfies both \\(P=0\\) and \\(Q=0\\). "
      "We then substitute the value of \\(v\\) and solve both polynomials to find the common \\(u\\) values. "

    p $ do
      "If Symtegration is not able to find real number solutions, "
      "especially for two polynomials with two variables, "
      "then it cannot derive an integral of a rational function in terms of only real functions. "
      "In this case, it will fall back to using complex logarithms in the integral. "

  section ! A.id "together" $ do
    h2 "Tying it all together"

    p "In conclusion, to integrate a rational function \\(f\\) with rational number coefficients, Symtegration "

    ol $ do
      li $ do
        "Applies "
        a ! href "#hermite" $ "Hermite reduction"
        " to derive \\(g\\) and \\(h\\) such that \\(\\int f \\, dx = g + \\int h \\, dx\\). "

      li $ do
        "For \\(h = \\frac{p}{q}\\), uses polynomial division to divide \\(p\\) by \\(q\\) to derive "
        "\\(h = s + \\frac{r}{q}\\), so that the degre of \\(r\\) is less than the degree of \\(q\\). "

        ul $ do
          li $ do
            "Deriving the integral for the polynomial \\(s\\) is straightforward, "
            "so the only thing remaining is to integrate \\(\\frac{r}{q}\\). "

      li $ do
        "Integrates \\(\\frac{r}{q}\\) into a "
        a ! href "#logterms" $ "sum of logarithms"
        " and turns "
        a ! href "#complextoreal" $ "complex logarithms into real functions"
        "."

        ul $ do
          li $ do
            "If we are not able to turn complex logarithms into real functions, "
            "we fall back to using the complex logarithms directly. "
