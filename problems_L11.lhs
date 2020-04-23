\begin{code}
-- P: area of rectangle
numbersFrom x = [x..(x+100)]
multlist l1 l2 = zipWith (*) l1 l2
allfactorials = 1:1:(multlist (numbersFrom 1) (tail allfactorials))
\end{code}