(* max from a list *)
fun my_max [] = 0
| my_max [x] = x
| my_max (x::xs) = 
	if x > (my_max xs) then x
	else my_max xs;


(* factorial *)
fun fact 0 = 1
| fact n = n * fact (n-1);

(* concatenare *)
fun cat [] l2 = l2
| cat (x::xs) y = x :: cat xs y;
