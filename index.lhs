\documentclass{beamer}

\usepackage[utf8]{inputenc}
\usepackage{minted}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\long\def\ignore#1{}

%% Hiden Haskell stuff required by GHC to compile properly
\ignore{
\begin{code}
import Control.Monad
import Data.Array.IO
import Data.Foldable
main = undefined
\end{code}
}

\title{IO}
\author{Alexey Kutepov}
\institute{Tsoding}
\date{2018}

\begin{document}

\frame{\titlepage}

\begin{frame}
  %% Blank
\end{frame}

%% TODO: introduce yourself

\begin{frame}[fragile]
  \begin{center}
    \Huge What is IO?
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \frametitle{IO is a container}
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

\begin{frame}[fragile]
  \frametitle{IO can encapsulate an action with a side effect}
\begin{minted}{haskell}
getLine :: IO String
putStrLn :: String -> IO () -- () is basically void type
readFile :: FilePath -> IO String
system :: String -> IO ExitCode
\end{minted}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Several IO actions can be sequenced with a do-block}
\begin{code}
-- do-block itself is IO
whatIsYourName :: IO ()
whatIsYourName =
    do putStrLn "What is your name?"
       name <- getLine  -- arrow unwraps IO
       putStrLn ("Hello " ++ name)
\end{code}
\end{frame}

%% \input{rulesofio.lhs}
\begin{frame}[fragile]
  \frametitle{Quick Sort}
\begin{code}
qsort [] = []
qsort (pivot:rest) = qsort left ++ [pivot] ++ qsort right
  where left  = [x | x <- rest, x <= pivot]
        right = [x | x <- rest, x >  pivot]
\end{code}
\end{frame}

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
  \begin{center}
    \Huge But what IO actually is?
  \end{center}
\end{frame}

%% TODO: World transformer POV of IO

\end{document}
