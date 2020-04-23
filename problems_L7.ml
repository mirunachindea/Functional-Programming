(*P6*)
(*Chindea Miruna*)
fun member (_,[])  = false
| member (x,y::ys) = if x = y then true
		     else member (x,ys);
		     
fun reverse []  = [] 
| reverse (x::xs) = reverse xs @ [x]; 

fun succ (x, [])      = []
 | succ (x,(y,z)::lp) = if x = y then z::succ(x,lp)
                        else succ(x,lp); 

fun bf([],dag,vis)   = reverse(vis)
 | bf(x::xs,dag,vis) = 
      if member(x,vis) then bf(xs, dag, vis)
      else bf(xs@succ(x,dag), dag, x::vis); 

val dag=[("a","b"),("a","c"),("a","d"),("b","e"),("c","f"),("d","e"),
		("e","f"),("e","g")]; 
