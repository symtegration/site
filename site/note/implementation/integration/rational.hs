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

  section ! A.id "representation" $ do
    h2 "Representing rational functions"

    p $ do
        "While the symbolic representation of mathematical expressions in Symtegration faithfully "
        "represents the semantics of mathematical expressions written in Haskell, "
        "we also use a special representation for polynomials to support the many "
        "algorithsm for polynomials that are useful for symbolic integration. "
        "The "
        symtegrationModule ["Polynomial"]
        " module includes the general operations that our polynomial representations need to use "
        "and various algorithms for polynomials such as finding the greatest common divisor.  The "
        symtegrationModule ["Polynomial", "Indexed"]
        " module defines a concrete representation for polynomminals. "

  section ! A.id "hermite" $ do
    h2 "Hermite reduction"

  section ! A.id "logterms" $ do
    h2 "Integrating the logarithmic terms"

  section ! A.id "complextoatan" $ do
    h2 "Switching from complex logarithms to inverse tangents"

  section ! A.id "complextoreal" $ do
    h2 "Switching from complex logarithms to real functions"

  section ! A.id "together" $ do
    h2 "Tying it all together"
