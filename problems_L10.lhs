\begin{code}

-- P3
leapyears [] = []
leapyears (x:xs) = 
	if x `mod` 4 == 0 then
		if x `mod` 100 == 0 then
			if x `mod` 400 == 0 then
				x : leapyears xs
			else 
				leapyears xs
		else 
			x : leapyears xs
	else leapyears xs

-- P5
average l = (sum l) `div` (length l)
sumsq [] = 0
sumsq (x:xs) = x*x + sumsq xs
stdDev x = sqrt(fromIntegral ((sumsq x) `div` (length x) - ((average x) * (average x))))

-- P6
combinations n = comb [1..n]
comb []  = [[]]
comb (x:xs) = comb xs ++ map (x:) (comb xs)

-- P4
data Tree = Nil | Node Integer Tree Tree
root = Node 1 Nil Nil
tree = Node 0 (Node 1 (Node 3 (Node 7 Nil Nil) (Node 8 Nil Nil)) 
					  (Node 4 Nil Nil)) 
			  (Node 2 (Node 5 Nil Nil) (Node 6 Nil Nil))

preorder::Tree->[Integer]
preorder Nil = []
preorder (Node val left right) = ([val]++preorder(left))++preorder(right)

\end{code}