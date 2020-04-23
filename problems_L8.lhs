\begin{code}
--Chindea Miruna
--P2
commonFactorsAcc x y acc = 
			if (x == acc) then []
			else 
				if (x `mod` acc) == 0 then
					if (y `mod` acc) == 0 then 
						acc : commonFactorsAcc x y (acc + 1)
					else
						commonFactorsAcc x y (acc + 1)
				else 
					commonFactorsAcc x y (acc + 1)

commonFactors x y = 
	if ((x == 0) || (y == 0)) then []
	else if x > y then
		commonFactorsAcc y x 1 
		else 
		commonFactorsAcc x y 1
		
--P4
--inner product
innerProduct [] [] = 0
innerProduct (x:xs) (y:ys) = (x * y) + innerProduct xs ys

\end{code}