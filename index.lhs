\documentclass{beamer}

\usepackage[utf8]{inputenc}
\usepackage{minted}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\long\def\ignore#1{}

\ignore{
\begin{code}
import Control.Monad
import Data.Array.IO
import Data.Foldable
\end{code}
}

\title{IO}
\author{Alexey Kutepov}
\institute{Tsoding}
\date{2018}

\begin{document}

\frame{\titlepage}

\begin{frame}[fragile]
  \begin{center}
    \Huge What is IO?
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \frametitle{IO is a Container}
\begin{code}
-- Comments just like in SQL

-- Signature
int :: Int
-- Definition
int = 42

intIO :: IO Int          -- (IO Int) holds Int
intIO = return 42        -- return wraps Int into IO

intString :: IO String   -- (IO String) holds String
intString = return "Foo" -- return wraps String into IO
\end{code}
\end{frame}

%% \input{rulesofio.lhs}

\begin{frame}[fragile]
\frametitle{Bubble Sort}
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

\begin{frame}
But what is IO?
\end{frame}

\input{io.lhs}

\end{document}
