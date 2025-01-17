import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p $ do
    "This page explains the general integration techniques used by Symtegration "
    "which work in with direct integration methods. "
    "Together, they can integrate functions that the direct methods cannot integrate directly. "
    "For example, the "
    a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Trigonometric.html" $
      "Symtegration" <> wbr <> ".Integration" <> wbr <> ".Trigonometric"
    " module cannot integrate "
    inlineMath $ toLaTeX $ sin ("a" * x)
    " by itself, but it can be integrated to "
    inlineMath $ integrated $ sin ("a" * x)
    " by using the "
    a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Substitution.html" $
      "Symtegration.Integration.Substitution"
    " module on top of the "
    a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Trigonometric.html" $
      "Symtegration.Integration.Trigonometric"
    " module."

  p $ do
    "Symtegration does not integrate by parts at this time. "
    "Further development of Symtegration is focused on systematic approaches first. "

  section $ do
    h2 ! A.id "constant" $ "Integrating with constant factors"

    p $ do
      "The "
      a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Term.html" $
        "Symtegration.Integration.Term"
      " module can integrate a single additive term by separating out a constant factor, "
      "integrating the remaining factors, and then multiplying the constant factor with the integral. "

    displayMath "\\int a f \\, dx = a \\int f \\, dx"

  section $ do
    h2 ! A.id "term" $ "Integrating by term"

    p $ do
      "The "
      a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Sum.html" $
        "Symtegration.Integration.Sum"
      " module integrates a sum by integrating each summand separately and adding the integrals together."

    displayMath "\\int \\left( \\sum f_i \\right) \\, dx = \\sum \\left( \\int f_i \\, dx \\right)"

  section $ do
    h2 ! A.id "substitution" $ "Integration by substitution"

    p $ do
      "The "
      a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Substitution.html" $
        "Symtegration.Integration.Substitution"
      " module integrates by substitition. "

    p "If \\(f' = \\frac{df}{dx}\\) and \\(g' = \\frac{dg}{dx}\\), then by the chain rule we have"

    displayMath $ "\\frac{d f(g(x))}{dx} = g'(x) f'(g(x))"

    p "From this, it follows that"

    displayMath $ "\\int g'(x) f'(g(x)) \\, dx = f(g(x))"

    p $ do
      "If we have a function which is the multiplication of two other functions, "
      "we could try differentiating the argument to one of the functions to see if it is "
      "the same as the other function, and if so, we can integrate by substitution. "

    p $ do
      "To get this to work, we need to be able to differentiate functions, for which we use "
      a ! href "https://hackage.haskell.org/package/ad" $ "AD"
      ". We also need to compare whether two separate representations of functions represent equivalent functions. "
      "This cannot always be done perfectly for arbitrary functions, "
      "but we can rewrite representations to make it more likely that equivalent functions "
      "have syntactically equal representations. "

    p $ do
      "The variable multiplied by and added to a constant is a special case of "
      "integration by substitution, with \\(g = ax+b\\)."

    displayMath $ "\\int a f'(ax + b) \\, dx = f(ax+b)"

  section $ do
    h2 ! A.id "parts" $ "Integration by parts"

    p "At this time, Symtegration does not integrate by parts, although it is still explained in this section. "

    p "By the product rule, the derivative of the product of two functions \\(f\\) and \\(g\\) is as follows."

    displayMath "\\frac{d(fg)}{dx} = g \\frac{df}{dx} + f \\frac{dg}{dx}"

    p "A minor rearrangement gives"

    displayMath "g \\frac{df}{dx} = \\frac{d(fg)}{dx} - f \\frac{dg}{dx}"

    p "If we know how to integrate \\(f \\frac{dg}{dx} \\, dx\\), then we can integrate \\(g \\frac{df}{dx} \\, dx\\)."

    displayMath $
      mconcat
        [ "\\begin{align*}\n",
          "\\int g \\frac{df}{dx} \\, dx",
          " & = \\int \\left( \\frac{d(fg)}{dx} - f \\frac{dg}{dx} \\right) \\, dx \\\\\n",
          " & = \\int \\frac{d(fg)}{dx} \\, dx - \\int f \\frac{dg}{dx} \\, dx \\\\\n",
          " & = f g - \\int f \\frac{dg}{dx} \\, dx\n",
          "\\end{align*}"
        ]

    p $ do
      "If we are given a function \\(F(x) G(x)\\), we can try integrating this by guessing an \\(f\\) "
      "such that \\(F = \\frac{df}{dx}\\), and hoping that \\(\\int f \\frac{dG}{dx} \\, dx\\) is "
      "easier to integrate.  If this does not work, we could try again with \\(G\\). "
      "Basically, we may be able to derive the integral of the original function from two easier integrals. "
