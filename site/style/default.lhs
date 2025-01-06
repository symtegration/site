\subsection{Preamble}

\begin{code}
import Clay
import Clay.Media qualified as Media
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
  genericStyle
  codeStyle
  mediaStyles
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
\end{code}

\begin{code}
bodyStyle :: Css
bodyStyle = do
  body ? do
    marginTop $ em 1
    marginBottom $ em 1
    marginLeft $ em 2
    marginRight $ em 2
\end{code}

\begin{code}
headingStyle :: Css
headingStyle = do
  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
    textAlign center

  h1 ? do
    fontFamily ["Quintessential"] [serif]
    fontSize $ em 3
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
    opacity $ 0.75
\end{code}

\begin{code}
codeStyle :: Css
codeStyle = do
  div # ".sourceCode" ? do
    borderStyle solid
    borderWidth $ px 1
    marginTop $ em 1
    marginBottom $ em 1
    marginRight $ em 3
    marginLeft $ em 3
    sym padding $ em 0.5
\end{code}

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
\end{code}

\begin{code}
darkColorScheme :: Css
darkColorScheme = do
  html ? do
    color white
    backgroundColor black

  a # link ? color cyan
  a # visited ? color pink
\end{code}
