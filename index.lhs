\documentclass{beamer}

\usepackage[utf8]{inputenc}
\usepackage{minted}
\usepackage{pgfpages}
\usepackage[absolute,overlay]{textpos}
\usepackage{color}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\long\def\ignore#1{}

\newcommand{\nextslide}[1]{%
\begin{textblock}{1.0}(15,15)%
{\color{gray} \tiny #1}%
\end{textblock}%
}

%% Hiden Haskell stuff required by GHC to compile properly
\ignore{
\begin{code}
import Control.Monad
import Data.Array.IO
import Data.Foldable
import System.IO.Unsafe
main = undefined
\end{code}
}

\title{IO}
\author{Alexey Kutepov}
\date{2018}

\begin{document}

\frame{\titlepage}

\begin{frame}
  %% Blank
\nextslide{who?}
\end{frame}

%% TODO: introduce yourself?
\begin{frame}
\frametitle{Who am I?}
\pause
\begin{itemize}
  \itemsep20pt
\item Alexey Kutepov \pause
\item Unemployed \pause
\item Occasional Freelancer \pause
\item Jack of All Trades
\end{itemize}
\nextslide{tso}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge What is Pure FP?
  \end{center}
  \nextslide{wipp?}
\end{frame}

\begin{frame}[fragile]
  \frametitle{What is Procedural Programming?}
  \pause
  \begin{minted}{c}
    proc() {
      proc1();
      proc2();
      proc3();
      ...
    }
  \end{minted}
  \nextslide{wipfp?}
\end{frame}

\begin{frame}[fragile]
  \frametitle{What is Pure Functional Programming?}
  \pause
  \begin{itemize}
  \item \verb|f1(f2(f3(...)))| \pause
  \item $f_1 \circ f_2 \circ f_3 \circ \ldots \circ f_n $
  \end{itemize}
  \nextslide{wiio?}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge What is IO?
  \end{center}
  \nextslide{cont}
\end{frame}

\begin{frame}[fragile]
  \frametitle{IO is a container}
  \pause
\begin{code}
-- Comments just like in SQL
\end{code}
\pause
\begin{code}
int :: Int               -- Signature
int = 42                 -- Definition
\end{code}
\pause
\begin{code}
intIO :: IO Int          -- (IO Int) holds Int
intIO = return 42        -- return wraps Int into IO
\end{code}
\pause
\begin{code}
intString :: IO String   -- (IO String) holds String
intString = return "Foo" -- return wraps String into IO
\end{code}
\nextslide{action}
\end{frame}

\begin{frame}[fragile]
  \frametitle{IO can encapsulate an action with a side effect}
\begin{minted}{haskell}
getLine :: IO String
putStrLn :: String -> IO () -- () is "void" type
readFile :: FilePath -> IO String
system :: String -> IO ExitCode
\end{minted}
\nextslide{do}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Several IO actions can be sequenced with a do-block}
\begin{code}
whatIsYourName :: IO ()  -- do-block itself is IO
whatIsYourName =
    do putStrLn "What is your name?"
       name <- getLine   -- arrow unwraps IO
       putStrLn ("Hello " ++ name)
\end{code}
\nextslide{qsort}
\end{frame}

%% \input{rulesofio.lhs}
\begin{frame}[fragile]
  \frametitle{Quick Sort}
\begin{code}
qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = qsort left ++ [p] ++ qsort right
  where left  = [x | x <- xs, x <= p]
        right = [x | x <- xs, x >  p]
\end{code}
\nextslide{bubble}
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
\nextslide{wiio?}
\end{frame}

\begin{frame}
  \begin{center}
    \Huge But what IO actually is?
  \end{center}
\end{frame}

\begin{frame}[fragile]
\begin{code}
-- TODO: explain what is `deriving Show`
data World = World deriving Show
\end{code}
\end{frame}

\begin{frame}[fragile]
\begin{code}
printStr :: String -> World -> World
-- TODO: printStr implementation should not be visible
printStr s w = unsafePerformIO (putStrLn s >> return w)

readStr :: World -> (World, String)
-- TODO: readStr implementation should not be visible
readStr w = unsafePerformIO (getLine >>= (\s -> return (w, s)))
\end{code}
\end{frame}

\begin{frame}[fragile]
\begin{code}
-- TODO: whatIsYourPureName doesn't perform actions in the right order
whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4
    where w2         = printStr "What is your name?" w1
          (w3, name) = readStr w2
          w4         = printStr ("Hello " ++ name) w3
\end{code}
\end{frame}

%% TODO: Introduce branching world problem

\end{document}
