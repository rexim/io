\documentclass{beamer}

\usepackage[utf8]{inputenc}
\usepackage{minted}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\long\def\ignore#1{}

\title{IO}
\author{Alexey Kutepov}
\institute{Tsoding}
\date{2018}

\begin{document}

\frame{\titlepage}

\begin{frame}[fragile]
\frametitle{Bubble Sort}
\ignore{
\begin{code}
import Control.Monad
import Data.Array.IO
import Data.Foldable
\end{code}
}
\begin{code}
bubbleSort :: IOArray Int Int -> IO ()
bubbleSort xs = do
  (low, high) <- getBounds xs
  for_ [low .. high - 1] $ \i ->
      for_ [i + 1 .. high] $ \j -> do
          x <- readArray xs i
          y <- readArray xs j
          when (x > y) $
            do writeArray xs i y
               writeArray xs j x
\end{code}
\end{frame}

\begin{frame}[fragile]
\frametitle{Rules of IO}

\begin{enumerate}
\item You can never unwrap IO
\item \verb|do|-block returns IO
\item
\end{enumerate}

\end{frame}

\end{document}
