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

\begin{code}
 mnmInt :: [Int] -> Int
 mnmInt [] = error "empty list"
 mnmInt [x] = x
 mnmInt (x:xs) = min x (mnmInt xs)

 mxmInt :: [Int] -> Int
 mxmInt [] = error  "empty list"
 mxmInt [x] = x
 mxmInt (x:xs) = max x (mxmInt xs)
\end{code}

{\bf Exercise 1.11} Define a function \texttt{removeFst} that removes the first
occurrence of an integer $m$ from a list of integers.  If $m$ does not occur in
the list, the list remains unchanged.

\begin{code}
 removeFst :: Int -> [Int] -> [Int]
 removeFst m [x] | m == x    = []
                 | otherwise = [x]
 removeFst m (x:xs) | m == x    = xs
                    | otherwise = x : removeFst m xs

 srtInts :: [Int] -> [Int]
 srtInts [] = []
 srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs
\end{code}

{\bf Exercise 1.13} Write a function \texttt{count} for counting the number of
occurrences of a character in a string.

\begin{code}
 count :: Char -> String -> Int
 count c [] = 0
 count c (x:xs) | c == x    = 1 + count c xs
                | otherwise = count c xs
\end{code}

{\bf Exercise 1.14} Write a function \texttt{blowup} that converts
$a_{1}a_{2}a_{3}...$ to $a_1 a_2 a_2 a_3 a_3 a_3 ...$.

\begin{code}
 blowdown :: String -> String
 blowdown [x]    = [x]
 blowdown (x:xs) = take (length (x:xs)) (repeat x) ++ blowdown xs

 blowup :: String -> String
 blowup x = (reverse (blowdown (reverse x)))
\end{code}

{\bf Exercise 1.15} Write a function \texttt{srtStrings} that sorts a list of
Strings in alphabetical order.

\begin{code}
 mnmString :: [String] -> String
 mnmString [] = error "empty list"
 mnmString [x] = x
 mnmString (x:xs) = min x (mnmString xs)

 removeFstString :: String -> [String] -> [String]
 removeFstString m [x] | m == x     = []
                       | otherwise  = [x]
 removeFstString m (x:xs) | m == x      = xs
                          | otherwise   = x : removeFstString m xs

 srtStrings :: [String] -> [String]
 srtStrings [] = []
 srtStrings xs = m : (srtStrings (removeFstString m xs)) where m = mnmString xs

\end{code}

{\bf Exercise 1.17} Write a function \texttt{substring} that checks whether
\texttt{str1} is a substring of \texttt{str2}.

\begin{code}
 prefix :: String -> String -> Bool
 prefix [] ys = True
 prefix (x:xs) [] = False
 prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

 substring :: String -> String -> Bool
 substring (x:xs) [] = False
 substring (x:xs) (y:ys) = (prefix (x:xs) (y:ys) || (substring (x:xs) ys))
\end{code}
\end{document}
