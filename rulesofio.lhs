\begin{frame}<1>[fragile,label=rules]
\frametitle{Rules of IO}

\begin{enumerate}
\item IO encapsulates an action with a side effect
  \pause
\item Several IO-s can be sequenced with a do-block
  \pause
\item do-block itself has IO type
  \pause
\item return wraps pure values in IO
  \pause
\item You can only unwrap IO inside of a do-block
\end{enumerate}
\end{frame}

\begin{frame}[fragile,noframenumbering]
  \frametitle{IO encapsulates an action with a side effect}
  \begin{minted}{haskell}
    getLine :: IO String
    putStrLn :: String -> IO ()
    readFile :: FilePath -> IO String
    system :: String -> IO ExitCode
  \end{minted}
\end{frame}

\againframe<2>{rules}

\begin{frame}[fragile,noframenumbering]
  \frametitle{Several IO-s can be sequenced with a do-block}
\begin{code}
whatIsYourName =
    do putStrLn "What is your name?"
       name <- getLine
       putStrLn ("Hello " ++ name)
\end{code}
\end{frame}

\againframe<3>{rules}

\begin{frame}[fragile,noframenumbering]
  \frametitle{do-block itself has IO type}
\begin{code}
program :: IO ()
program = whatIsYourName
\end{code}
\end{frame}

\againframe<4>{rules}

\begin{frame}[fragile,noframenumbering]
  \frametitle{return wraps pure values in IO}
\begin{code}
answer :: IO Int
answer = return 42
\end{code}
\end{frame}

\againframe<5>{rules}

\begin{frame}[fragile,noframenumbering]
  \frametitle{You can only unwrap IO inside of a do-block}
\begin{code}
whatIsYourName2 = do
  putStrLn "What is your name?"
  name <- getLine     -- Use <- if IO type
  let hello = "Hello" -- Use let if just type
  space <- return " " -- Wrap with return to avoid let
  putStrLn (hello ++ space ++ name)
\end{code}
\end{frame}

\againframe<6>{rules}
