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

%% TODO: put Bubble Sort after the Rules of IO to shock the audience
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

%% TODO: Definition of IO before Rules of IO section

\input{rulesofio.lhs}

\begin{frame}
But what is IO?
\end{frame}

\input{io.lhs}

\end{document}
