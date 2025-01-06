\subsection{Preamble}

\begin{code}
import Clay
import Clay.Media qualified as Media
import Data.Text (Text)
\end{code}

\begin{code}
import Prelude hiding (div)
\end{code}

\begin{code}
main :: IO ()
main = do
  putSyntaxImports
  putCss defaultStyle
\end{code}

\begin{code}
narrowWidth :: Size LengthUnit
narrowWidth = em 30

wideWidth :: Size LengthUnit
wideWidth = em 60
\end{code}

\subsection{Imports}

\begin{code}
putSyntaxImports :: IO ()
putSyntaxImports = do
  putStrLn "@import \"syntax-light.css\" all and (prefers-color-scheme: light);"
  putStrLn "@import \"syntax-dark.css\" all and (prefers-color-scheme: dark);"
\end{code}

\subsection{Style}

\begin{code}
defaultStyle :: Css
defaultStyle = do
  fontDefinitions
  genericStyle
  codeStyle
  mediaStyles
\end{code}

\subsection{Definition of fonts}

\begin{code}
fontDefinitions :: Css
fontDefinitions = do
  importUrl "https://fonts.googleapis.com/css2?family=Geist+Mono:wght@100..900&family=Noto+Sans:ital,wght@0,100..900;1,100..900&family=Nova+Mono&display=swap"
\end{code}

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

  bodyStyle
  headingStyle
  footerStyle
  listsStyle
\end{code}

\begin{code}
bodyStyle :: Css
bodyStyle = do
  body ? do
    marginTop $ em 1
    marginBottom $ em 1
    marginLeft $ em 2
    marginRight $ em 2

    fontFamily [notoSans] [sansSerif]
\end{code}

\begin{code}
headingStyle :: Css
headingStyle = do
  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
    textAlign $ alignSide sideLeft
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
    textAlign center
    marginTop $ em 1
    borderTopStyle solid
    borderTopWidth $ px 1

    fontSize $ em 0.75
    opacity 0.75
\end{code}

\begin{code}
listsStyle :: Css
listsStyle = do
  li |+ li ? marginTop (em 0.75)
  li |> (ul <> ol) ? marginTop (em 0.75)
\end{code}

\subsection{Code style}

\begin{code}
codeStyle :: Css
codeStyle = do
  div # ".sourceCode" <> pre # ".shell" ? do
    borderStyle solid
    borderWidth $ px 1
    marginTop $ em 1
    marginBottom $ em 1
    marginRight $ em 3
    marginLeft $ em 3
    sym padding $ em 0.5

    fontFamily [geistMono] [monospace]
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
