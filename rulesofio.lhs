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

\begin{frame}[noframenumbering]
  \frametitle{IO encapsulates an action with a side effect}
\end{frame}

\againframe<2>{rules}

\begin{frame}[noframenumbering]
  \frametitle{Several IO-s can be sequenced with a do-block}
\end{frame}

\againframe<3>{rules}

\begin{frame}[noframenumbering]
  \frametitle{do-block itself has IO type}
\end{frame}

\againframe<4>{rules}

\begin{frame}[noframenumbering]
  \frametitle{return wraps pure values in IO}
\end{frame}

\againframe<5>{rules}

\begin{frame}[noframenumbering]
  \frametitle{You can only unwrap IO inside of a do-block}
\end{frame}

\againframe<6>{rules}
