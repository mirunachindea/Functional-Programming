\begin{code}
-- Chindea Miruna
-- P2
safetail [] = []
safetail (x:xs) = xs

-- P3
-- functie care verifica daca o lista e palindrom
-- ex palindromeL ["a man", "a plan", "a canal", "panama"] True

-- concatenam stringurile
flatten [] = []
flatten (x:xs) = x ++ flatten xs; 

-- scoatem spatiile
removespace "" = ""
removespace (x:xs) = if x == ' ' then removespace xs
						else x : removespace xs

-- inversam stringul
rev s = rev_acc [] s
	where
		rev_acc acc [] = acc
		rev_acc acc (x:xs) = rev_acc (x:acc) xs

-- verificam daca stringul e palindrom
palindrome s = equal s (rev s)
	where
		equal "" "" = True
		equal (x:xs) (y:ys) = if x==y then equal xs ys
								else False
-- verificam daca lista e palindrom
palindromeL l = palindrome rem
	where 
		rem = removespace flat
		flat = flatten l

-- P4
-- function that eliminates every nth element of a list 
decimate l n = decimate_acc l n 1
	where 
		decimate_acc [] n acc = []
		decimate_acc (x:xs) n acc = 
			if (mod acc n) == 0 then
				decimate_acc xs n (acc+1)
			else
				x : decimate_acc xs n (acc+1)


-- P5
-- function that adds 2 big numbers given as lists of digits
addBigs l1 l2 = 
	let (f, r) = addBigs0 l1 l2 in
	if f == 1 then 1 : r else r
	where
		addBigs0 [] [] = (0, [])
		addBigs0 (x:xs) (y:ys) = 
			let (f, r) = addBigs0 xs ys in
				if (x + y + f) > 9 then
					(1, (x + y + f - 10) : r)
				else
					(0, (x + y) : r)

-- P7
member node [] = False
member node (x:xs) = 
	if x == node then True
    else member node xs

revl l = revl_acc [] l
       where 
          revl_acc acc [] = acc
          revl_acc acc (x:xs) = revl_acc (x:acc) xs

successor node [] = []
successor node ((x,y):xs) = 
	if x == node then y : (successor node xs)
    else successor node xs
    
difference [] list = []
difference (x:xs) list = 
	if (member x list) then difference xs list 
	else x : (difference xs list)

df [] dag visited = revl visited
df (x:xs) dag visited = 
	df ((difference (successor x dag) (visited ++ xs)) ++ xs) dag (x:visited)

val dag=[("a","b"),("a","c"),("a","d"),("b","e"),("c","f"),("d","e"),("e","f"),("e","g")]
\end{code}