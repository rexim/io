\documentclass{beamer}

\usepackage[utf8]{inputenc}
\usepackage{minted}
\usepackage{pgfpages}
\usepackage[absolute,overlay]{textpos}
\usepackage{color}
\usepackage{qrcode}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\long\def\ignore#1{}

%% Hiden Haskell stuff required by GHC to compile properly
\ignore{
\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Data.Array.IO
import Data.Foldable
import System.IO.Unsafe
infixl 1  >>>=
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

\end{frame}

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

\end{frame}

\begin{frame}[fragile]
  \frametitle{Project Tsoding}
  \begin{center}
    \Huge {\color{gray} @}tsoding
  \end{center}

  \begin{textblock}{1.0}(1.5, 7)
    \includegraphics[scale=0.4]{imgs/GitHub-Mark-120px-plus.png}
  \end{textblock}

  \begin{textblock}{1.0}(5, 2)
    \includegraphics[scale=0.4]{imgs/Glitch_Purple_RGB.png}
  \end{textblock}

  \begin{textblock}{1.0}(8, 10)
    \includegraphics[scale=0.2]{imgs/Twitter_Logo_Blue.png}
  \end{textblock}

  \begin{textblock}{1.0}(12, 5)
    \includegraphics[scale=0.06]{imgs/youtube.png}
  \end{textblock}

\end{frame}

\begin{frame}[fragile]
  \frametitle{HaskellRank}
  \begin{center}
    \qrcode[height=2in]{https://bit.ly/haskellrank}

    \url{https://bit.ly/haskellrank}
  \end{center}

\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge What is Pure FP?
  \end{center}

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

\end{frame}

\begin{frame}[fragile]
  \frametitle{What is Pure Functional Programming?}
  \pause
  \begin{itemize}
  \item \verb|f1(f2(f3(...)))| \pause
  \item $f_1 \circ f_2 \circ f_3 \circ \ldots \circ f_n $
  \end{itemize}

\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge What is IO?
  \end{center}

\end{frame}

\begin{frame}[fragile]
  \frametitle{IO is a container}
  \pause
\begin{code}
-- Comments just like in SQL
\end{code}
\pause
\hrule
\begin{code}
int :: Int               -- Signature
int = 42                 -- Definition
\end{code}
\pause
\hrule
\begin{code}
intIO :: IO Int          -- (IO Int) holds Int
intIO = return 42        -- return wraps Int into IO
\end{code}
\pause
\hrule
\begin{code}
intString :: IO String   -- (IO String) holds String
intString = return "Foo" -- return wraps String into IO
\end{code}

\end{frame}

\begin{frame}[fragile]
  \frametitle{IO can encapsulate an action with a side effect}
\begin{minted}{haskell}
getLine :: IO String
putStrLn :: String -> IO () -- () is "void" type
readFile :: FilePath -> IO String
system :: String -> IO ExitCode
\end{minted}

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

\end{frame}

\begin{frame}[fragile]
  \frametitle{Quick Sort}
\begin{code}
qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = qsort left ++ [p] ++ qsort right
  where left  = [x | x <- xs, x <= p]
        right = [x | x <- xs, x >  p]
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

\begin{frame}[fragile]
  \frametitle{Pure World}
  \pause
\begin{code}
data World = World
\end{code}
\ignore{
\begin{code}
-- Deriving Show for World under the hood. Show is very important for
-- forcing the actual evaluation of the World expression in REPL.
             deriving Show
\end{code}
}
\pause
\hrule
\begin{code}
printStr :: String -> World -> World
\end{code}
\ignore{
\begin{code}
printStr s !w = unsafePerformIO (putStrLn s >> return w)
\end{code}
}
\pause
\hrule
\begin{code}
readStr :: World -> (String, World)
\end{code}
\ignore{
\begin{code}
readStr !w = unsafePerformIO (getLine >>= (\s -> return (s, w)))
\end{code}
}

\end{frame}

\begin{frame}[fragile]
\begin{code}
whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4
    where
      w2         = printStr "What is your name?" w1
      (name, w3) = readStr w2
      w4         = printStr ("Hello " ++ name) w3
\end{code}

\end{frame}

\begin{frame}[fragile]
\frametitle{``Branching'' Problem}
\pause
\begin{code}
branch :: World -> (World, World)
branch w =
    (printStr "I love you!" w,
     printStr "I hate you!" w)
\end{code}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge How to solve ``Branching'' Problem
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge Uniqueness Typing
  \end{center}
\end{frame}

%% https://stackoverflow.com/questions/3850368/how-do-functional-languages-model-side-effects
%% https://clean.cs.ru.nl/Clean
%% http://www.mbsd.cs.ru.nl/publications/papers/cleanbook/CleanBookI.pdf
%% https://en.wikipedia.org/wiki/Uniqueness_type
\begin{frame}[fragile]
\frametitle{Uniqueness Type}
\pause
\begin{code}
-- Just a dummy marker to mark World as Unique
data Unique a = Unique a
\end{code}
\pause
\hrule
\begin{code}
-- printStr :: String -> World -> World
printStrU :: String -> Unique World -> Unique World
printStrU text (Unique w) = Unique (printStr text w)
\end{code}
\pause
\hrule
\begin{code}
-- If we had unique types this would not compile
branchUnique :: Unique World
             -> (Unique World, Unique World)
branchUnique w =
    (printStrU "I love you" w,
     printStrU "I hate you" w)
\end{code}
\end{frame}

\begin{frame}[fragile]
\frametitle{Uniqueness Type}
\begin{code}
noBranching :: Unique World -> Unique World
noBranching w1 = w3
    where w2 = printStrU "I love you" w1
          w3 = printStrU "I hate you" w2
\end{code}
\end{frame}

\begin{frame}[fragile]
\frametitle{Clean Programming Language}
\pause
\begin{minted}{clean}
module hello1
import StdEnv

Start :: *World -> *World
Start world
    // defining bindings
    # (console,world) = stdio world
    # console = fwrites "Hello World.\n" console
    # (ok,world) = fclose console world
    // condition branches
    | not ok = abort "Cannot close console.\n"
    | otherwise = world
\end{minted}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge Make World Inaccessible
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Currying}
  \pause
  \begin{minted}{javascript}
function f(a, b) {
    return a + b;
}
  \end{minted}
  \pause
  \begin{minted}{javascript}
f(10, 20); // => 30
  \end{minted}
  \pause
  \begin{minted}{javascript}
function g(a) {
    return function(b) {
        return a + b;
    };
}
  \end{minted}
  \pause
  \begin{minted}{javascript}
g(10);     // => function(b) { return 10 + b };
  \end{minted}
  \pause
  \begin{minted}{javascript}
g(10)(20); // => 30
  \end{minted}

\end{frame}

\begin{frame}[fragile]
\frametitle{Uncurrying}
\pause
  \begin{minted}{javascript}
function uncurry(f) {
    return function(a, b) {
        return f(a)(b)
    }
}
  \end{minted}
  \pause
  \hrule
  \begin{minted}{haskell}
uncurry :: (a -> b -> c) -> (a, b) -> c
  \end{minted}
\end{frame}

\begin{frame}[fragile]
  \frametitle{World Transformer}
  \pause
\begin{code}
-- Type synonym. It's the "typedef" of Haskell.
type WorldT a              = World -> (a, World)
\end{code}
\pause
\hrule
\begin{code}
readStrT :: WorldT String -- World -> (String, World)
readStrT = readStr
\end{code}
\pause
\hrule
\begin{code}
printStrT :: String       -- String
          -> WorldT ()    -- -> World -> ((), World)
printStrT s w = ((), printStr s w)
\end{code}
\pause
\hrule
\begin{code}
(>>>=) :: WorldT a        -- World -> (a, World)
       -> (a -> WorldT b) -- (a -> World -> (b, World))
       -> WorldT b        -- World -> (b, World)
\end{code}
\pause
\hrule
\begin{code}
wt >>>= f = uncurry f . wt
\end{code}
\end{frame}

\begin{frame}[fragile]
\frametitle{Interalias Composition}
\pause
\begin{minted}{haskell}
(>>>=) :: WorldT a -> (a -> WorldT b) -> WorldT b
wt >>>= f = uncurry f . wt
\end{minted}
\pause
\hrule
\begin{minted}{haskell}
wt               :: WorldT a
wt               :: World -> (a, World)
\end{minted}
\pause
\hrule
\begin{minted}{haskell}
f                :: a -> WorldT b
f                :: a -> World -> (b, World)
\end{minted}
\pause
\hrule
\begin{minted}{haskell}
(uncurry f)      :: (a, World) -> (b, World)
\end{minted}
\pause
\hrule
\begin{minted}{haskell}
(uncurry f . wt) :: WorldT b
(uncurry f . wt) :: World -> (b, World)
\end{minted}
\end{frame}

\begin{frame}[fragile]
\begin{code}
whatIsYourPureNameT :: WorldT ()
whatIsYourPureNameT =
    printStrT "What is your name?" >>>= \_ ->
    readStrT                       >>>= \name ->
    printStrT ("Hello " ++ name)
\end{code}
\end{frame}

\begin{frame}[fragile]
\frametitle{Integrating with do-notation}
\pause
We need to prove
\begin{itemize}
\pause
\item \verb|WorldT| is a Monad
\pause
\item \verb|>>>=| is equivalent to \verb|>>=|
\end{itemize}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Integrating with do-notation}
  \pause
\begin{code}
newtype WorldM a = WorldM { asT :: WorldT a }
                   deriving Functor
\end{code}
\pause
\hrule
\begin{code}
instance Applicative WorldM where
    pure x = WorldM (\w -> (x, w))
    wtf <*> wt = WorldM (asT wtf >>>= \f ->
                         asT wt  >>>= \x ->
                         asT $ pure $ f x)
\end{code}
\pause
\hrule
\begin{code}
instance Monad WorldM where
    wt >>= f = WorldM (asT wt >>>= asT . f)
\end{code}
\pause
\hrule
\begin{code}
printStrM :: String -> WorldM ()
printStrM = WorldM . printStrT
\end{code}
\pause
\hrule
\begin{code}
readStrM :: WorldM String
readStrM = WorldM readStrT
\end{code}
\end{frame}

\begin{frame}[fragile]
\begin{code}
whatIsYourPureNameM :: WorldM ()
whatIsYourPureNameM =
    do printStrM "What is your name?"
       name <- readStrM
       printStrM ("Hello " ++ name)
\end{code}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \Huge Questions?
  \end{center}
\end{frame}
\end{document}
