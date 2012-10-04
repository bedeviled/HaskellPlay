\documentclass[12pt]{article}

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1
    }
\long\def\ignore#1{}
\begin{document}

\ignore{
\begin{code}
module GS where
\end{code}
}

{\bf Exercise 1.1}  Find precedence order for $+,-,*,/, \wedge$ by experimenting
in the interpreter.
Answer:  Parentheses, Exponentiation, Multiplication, Division, add,
subtract... as usual.

\begin{code}
 divides :: Integer -> Integer -> Bool
 divides d n = rem n d == 0

 ldf :: Integer -> Integer -> Integer
 ldf k n | divides k n = k
         | k^2 > n    = n
         | otherwise   = ldf (k+1) n

 ld :: Integer -> Integer
 ld n = ldf 2 n
\end{code}

{\bf Exercise 1.4} Would replacing the $>$ in the second condition with a $>=$
change the meaning of the program?

No.  A change to the meaning of the program would imply a different output for
a given input.  The only input that could possibly affect the output is the
case when $k^2 = n$ and in this case the second test is never reached since the
first test is passed.

\begin{code}
 prime0 :: Integer -> Bool
 prime0 n | n < 1       = error "not a positive integer"
          | n == 1      = False
          | otherwise   = ld n == n
\end{code}

{\bf Exercise 1.6} The type declaration for \texttt{rem} should look like
\begin{code}%sample
rem :: Integer -> Integer -> Integer
\end{code}
\end{document}
