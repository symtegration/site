\subsection{Preamble}

We start off with the module imports.

\begin{code}
import Clay
import Clay.Media qualified as Media
import Data.Text (Text)
import Text.Pandoc.Highlighting (pygments, styleToCss, zenburn)
\end{code}

For this stylesheet, it is more convenient to use the name of the HTML elements
than to use the Haskell function names for these, so we hide these functions
from the Prelude.

\begin{code}
import Prelude hiding (div)
\end{code}

This outputs the stylesheet.

\begin{code}
main :: IO ()
main = do
  fontDefinitions
  putSyntaxStyles
  putCss defaultStyle
\end{code}

These are the lengths we define to determine what we shall decide
to be displays with wide widths or narrow widths.

\begin{code}
narrowWidth :: Size LengthUnit
narrowWidth = em 40

wideWidth :: Size LengthUnit
wideWidth = em 80
\end{code}

\subsection{Style}

\begin{code}
defaultStyle :: Css
defaultStyle = do
  genericStyle
  noteStyle
  codeStyle
  badgesStyle
  mediaStyles
\end{code}

\subsection{Definition of fonts}

This imports the external stylesheets necessary for the fonts that are used.
Since stylesheet imports must come first in a stylesheet,
this must be called first.

\begin{code}
fontDefinitions :: IO ()
fontDefinitions =
  putStrLn "@import url(https://fonts.googleapis.com/css2?family=Geist+Mono:wght@100..900&family=Noto+Sans:ital,wght@0,100..900;1,100..900&family=Nova+Mono&display=swap);"
\end{code}

These define names we use later when using fonts.  Using these instead of the font names
directly makes it less likely that we use a font that we do not import above.

\begin{code}
geistMono :: Text
geistMono = "Geist Mono"

notoSans :: Text
notoSans = "Noto Sans"

novaMono :: Text
novaMono = "Nova Mono"
\end{code}

\subsection{Generic style}

\begin{code}
genericStyle :: Css
genericStyle = do
  html ? do
    textRendering optimizeLegibility
    textAlign justify
    hyphens auto
    lineHeight $ unitless 1.75

  bodyStyle
  navStyle
  headingStyle
  footerStyle
  listsStyle
\end{code}

\begin{code}
bodyStyle :: Css
bodyStyle = do
  body ? do
    sym2 margin (em 1) (em 2)

    fontFamily [notoSans] [sansSerif]
\end{code}

\begin{code}
navStyle :: Css
navStyle = do
  nav # ".site" ? do
    marginTop $ em 1
    marginBottom $ em 1
    textAlign center
    textShadow (em 0.25) (em 0.25) (em 0.2) (rgba 125 125 125 0.5)
    fontVariant smallCaps
    fontSize $ pct 120
    a ? do
      marginLeft $ em 1
      marginRight $ em 1
\end{code}

\begin{code}
headingStyle :: Css
headingStyle = do
  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
    textAlign center
    fontFamily [novaMono] [sansSerif]

  h1 ? do
    fontSize $ em 3

  h2 ? do
    fontSize $ em 2

  h3 ? do
    fontSize $ em 1.6
    textDecorationLine underline
    textDecorationStyle dotted

  h4 ? do
    fontSize $ em 1.3
    textDecorationLine underline
    textDecorationStyle dashed

  h5 ? do
    fontSize $ em 1.1
    textDecorationLine underline
    textDecorationStyle solid

  h6 ? do
    fontSize $ em 1
    textDecorationLine underline
    textDecorationStyle double
\end{code}

\begin{code}
footerStyle :: Css
footerStyle = do
  footer ? do
    clear both
    textAlign center
    marginTop $ em 1
    paddingTop $ em 1
    borderTopStyle solid
    borderTopWidth $ px 1

    fontSize $ em 0.75
    opacity 0.75

    p ? do
      marginTop $ em 0.25
      marginBottom $ em 0.25
\end{code}

\begin{code}
listsStyle :: Css
listsStyle = do
  li |+ li ? marginTop (em 0.75)
  li |> (ul <> ol) ? marginTop (em 0.75)
\end{code}

\subsection{Note style}

This is the style for notes, which are notes on various subjects under the \texttt{note/} directory.
In particular, it contains styling for bylines, since these notes may have authors attributed
to them and dates they have been published or updated to help readers determine their currency.

\begin{code}
noteStyle :: Css
noteStyle = do
  div # ".byline" ? do
    textAlign center
    sym2 margin (em 1) auto
    borderLeftStyle solid
    borderLeftWidth bylineBarWidth
    borderLeftColor bylineBarColor
    borderRightStyle solid
    borderRightWidth bylineBarWidth
    borderRightColor bylineBarColor

    div ? do
      sym2 margin (em 0.1) (em 1)
      fontSize $ pct 75
      fontStyle italic
  where
    bylineBarWidth = px 5
    bylineBarColor = rgba 0 0 255 0.5
\end{code}

\subsection{Code style}

\begin{code}
codeStyle :: Css
codeStyle = do
  div # ".sourceCode" ? do
    borderStyle solid
    borderWidth $ px 1
    sym2 margin (em 1) (em 2)
    sym padding $ em 0.5

    fontFamily [geistMono] [monospace]
\end{code}

\subsection{Badges style}

\begin{code}
badgesStyle :: Css
badgesStyle = do
  div # ".badges" ? do
    textAlign center
\end{code}

\subsection{Media-specific styles}

\begin{code}
mediaStyles :: Css
mediaStyles = do
  query Media.all [Media.maxWidth narrowWidth] $ do
    body ? do
      marginLeft $ em 1
      marginRight $ em 1

  query Media.all [Media.minWidth wideWidth] $ do
    body ? do
      width wideWidth
      marginRight auto
      marginLeft auto

  query Media.all [Media.prefersColorScheme Media.light] lightColorScheme
  query Media.all [Media.prefersColorScheme Media.dark] darkColorScheme
\end{code}

\begin{code}
lightColorScheme :: Css
lightColorScheme = do
  html ? do
    color black
    backgroundColor white

  a # link ? color blue
  a # visited ? color purple

  h1 ? do
    color $ rgb 0 0 125

  headingColors $ \n -> rgb (n * 10) (n * 10) (120 + n * 10)
\end{code}

\begin{code}
darkColorScheme :: Css
darkColorScheme = do
  html ? do
    color white
    backgroundColor black

  a # link ? color cyan
  a # visited ? color pink

  h1 ? do
    color $ rgb 50 50 245

  headingColors $ \n -> rgb 100 100 (255 - n * 10)
\end{code}

\begin{code}
headingColors :: (Integer -> Color) -> Css
headingColors mapColor = do
  h2 ? fontColor (mapColor 2)
  h3 ? fontColor (mapColor 3)
  h4 ? fontColor (mapColor 4)
  h5 ? fontColor (mapColor 5)
  h6 ? fontColor (mapColor 6)
\end{code}

\begin{code}
putSyntaxStyles :: IO ()
putSyntaxStyles = do
  putStrLn ""
  putStrLn "/* Generated for syntax highlighting from skylighting. */"

  putStrLn "@media all and (prefers-color-scheme: light) {"
  putStrLn $ styleToCss pygments
  putStrLn "}"

  putStrLn "@media all and (prefers-color-scheme: dark) {"
  putStrLn $ styleToCss zenburn
  putStrLn "}"
\end{code}
