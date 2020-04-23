\begin{code}
-- max from a list
my_max [] = 0
my_max [x] = x
my_max (x:xs) = 
	if x > (my_max xs) then x
	else my_max xs
---------------------------------------------------------------------------

-- factorial 
fact 0 = 1
fact n = n * fact (n-1)
---------------------------------------------------------------------------

-- concatenare liste
cat [] l2 = l2
cat (x:xs) y = x : cat xs y
---------------------------------------------------------------------------

-- Scrie o functie care ia ca intrare o lista de numere intregi si returneaza o lista cu numerele pare mai mari decat media aritmetica a numerelor din lista initiala.
myfun l = filter (\x -> (even x) && (x > ma l)) l
	where
		ma l = (sum l) `div` (length l)
---------------------------------------------------------------------------

-- Functia my_union care returneaza uniunea a doua multimi date ca liste.
union2 l1 [] = l1
union2 [] l2 = l2
union2 x (y:ys) = if (member x y == True) then union2 x ys
				 else union2 (x++[y]) ys
	where 
		member [] y = False
		member (x:xs) y = if (x==y) then True 
		  				  else member xs y
---------------------------------------------------------------------------

-- Scrie o functie care are ca intrare un dreptunghi si returneaza aria acestuia. Dreptunghiul este o structura de date definita prin coordonatele punctelor luate in sens trigonometric
data Shape = Rectangle Float Float Float Float Float Float Float Float

area (Rectangle x1 y1 x2 y2 x3 y3 x4 y4) = 
	sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)) * 
	sqrt((x3-x2)*(x3-x2) + (y3-y2)*(y3-y2))

-- apelare 
-- d = Rectangle 1 2 3 4 5 6 7 8
-- area d
---------------------------------------------------------------------------

-- Scrie o functie care returneaza divizorii comuni (intr-o lista) a doua numere luate ca argumente.
cmf 0 n = []
cmf n 0 = []
cmf x y = if x<y then cmfacc x y 1 
		  else cmfacc y x 1
	where
		cmfacc x y acc = 
			if (x<acc) then []
			else if ((x`mod`acc==0) && (y`mod`acc==0)) then
						acc : cmfacc x y (acc + 1)
				else cmfacc x y (acc + 1)


---------------------------------------------------------------------------

-- Scrie functia decimate:
decimate [] n = []
decimate (x:xs) n = 
	if (x`mod`n==0) then decimate xs n
	else x : decimate xs n
----------------------------------------------------------------------

-- Defineste o functie pentru calculul deviatiei standard.
avrg l = (sum l) `div` length l
sumsq [] = 0
sumsq (x:xs) = x*x + sumsq xs
stdDev x = sqrt(fromIntegral ((sumsq x) `div` (length x) - ((avrg x) * (avrg x))))
---------------------------------------------------------------------------

-- Defineste un tip de date pentru un arbore. Scrie functiile pentru parcurgerea in ordine, inpreordine si in postordine, elementele fiind returnate intr-o lista. Scrie 2 exemple de apel pe un arbore oarecare al functiei in ordine/in preordine/ in postordine.
data Tree = Nil | Node Integer Tree Tree
root = Node 1 Nil Nil
tree = Node 0 (Node 1 (Node 3 (Node 7 Nil Nil) (Node 8 Nil Nil)) 
					  (Node 4 Nil Nil)) 
			  (Node 2 (Node 5 Nil Nil) (Node 6 Nil Nil))
preorder Nil = []
preorder (Node val left right) = ([val]++preorder (left))++preorder (right)

inorder Nil = []
inorder (Node val left right) = (inorder (left))++[val]++(inorder (right))

postorder Nil = []
postorder (Node val left right) = (postorder (left))++(postorder (right))++[val]

---------------------------------------------------------------------------
\end{code}
