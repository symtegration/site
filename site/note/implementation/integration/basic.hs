import Data.ByteString.Lazy qualified as ByteString
import Symtegration.Site.Content
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = ByteString.putStr $ renderHtml $ do
  p $ do
    "For most of the functions defined by the "
    a ! href "https://hackage.haskell.org/package/base/docs/GHC-Float.html#t:Floating" $ "Floating"
    " type class, their integrals are well-known, and we use what basically amounts to a table lookup "
    "to find their integrals.  While relying only on these directly would severely limit the "
    "variety of functions which can be integrated, these can be used with term by term integration "
    "or integration by substitution to significantly expand the range of functions which can be integrated. "

  p "This page lists out such functions and their integrals.  These are implemented in the following modules:"

  ul $ do
    li $
      a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Powers.html" $
        "Symtegration.Integration.Powers"

    li $
      a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Exponential.html" $
        "Symtegration.Integration.Exponential"

    li $
      a ! href "https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration-Trigonometric.html" $
        "Symtegration.Integration.Trigonometric"

  p $ do
    "See also "
    a ! href "/note/implementation/" $ "more implementation notes on Symtegration"
    "."

  section $ do
    h2 ! A.id "powers" $ "Integration of powers of the variable"

    p "For any expression \\(x^n\\), except for when \\(n=-1\\),"

    displayMath "\\int x^n \\, dx = \\frac{1}{n+1} x^{n+1}"

    p "\\(x^{-1} = \\frac{1}{x}\\) is the exception to the above pattern."

    displayMath $ integralExample (1 / x)

  section $ do
    h2 ! A.id "exponential" $ "Integration of exponentials"

    displayMath $ integralExample $ exp x

  section $ do
    h2 ! A.id "logarithm" $ "Integration of logarithms"

    displayMath $ integralExample $ log x

  section $ do
    h2 ! A.id "trigonometric" $ "Integration of trigonometric functions"

    displayMath $ integralExample $ sin x
    displayMath $ integralExample $ cos x
    displayMath $ integralExample $ tan x
    displayMath $ integralExample $ asin x
    displayMath $ integralExample $ acos x
    displayMath $ integralExample $ atan x
    displayMath $ integralExample $ sinh x
    displayMath $ integralExample $ cosh x
    displayMath $ integralExample $ tanh x
    displayMath $ integralExample $ asinh x
    displayMath $ integralExample $ acosh x
    displayMath $ integralExample $ atanh x
