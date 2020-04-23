\begin{code}
-- Chindea Miruna
-- p2
myplus x y = x + y

-- p3
sudan n x y = if n==0 then x+y
		  	  else if n>0 && y==0 then x
		  	  		else sudan (n-1) (sudan n x (y-1)) (y+sudan n x (y-1))

-- p4
-- nand
infixl 5 !& 
x !& y = not (x && y)

-- p5
-- take the first n-1 elements from a list of n elements
myinit [] = []
myinit [x] = []
myinit (x:xs) = x : myinit xs

-- p6
-- return last but 2 element of a list
lastbut2 (x:xs) = 
	if ((length xs) < 2) then 9999999 -- not enough elements
	else 
		if ((length xs) == 2) then x
		else (lastbut2 xs)

-- p7
-- check if list is palindrome
rev l = rev_acc [] l
	where
		rev_acc acc [] = acc
		rev_acc acc (x:xs) = rev_acc (x:acc) xs

palindrome l = equal l (rev l)
	where
		equal [] [] = True
		equal (x:xs) (y:ys) = if x == y then equal xs ys
								else False
\end{code}
