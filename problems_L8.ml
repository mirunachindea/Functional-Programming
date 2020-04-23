(*P2*)
(*Chindea Miruna*)
fun commonFactorsAcc x y acc = 
			if (x = acc) then []
			else 
				if ((x mod acc) = 0) then
					if ((y mod acc) = 0) then 
						acc::commonFactorsAcc x y (acc + 1)
					else
						commonFactorsAcc x y (acc + 1)
				else 
					commonFactorsAcc x y (acc + 1);

fun commonFactors x y = if x > y then
							commonFactorsAcc y x 1
						else
							commonFactorsAcc x y 1;

(*P3*)
exception BothZero;
exception AIsZero;

fun equation a b = 
	if ((a = 0) andalso (b = 0)) then
			raise BothZero
	else 
		if (a = 0) then
			raise AIsZero
		else
			(0.0 - (real b)) / (real a);

(*P4*)
fun innerProduct [] [] = 0
| innerProduct (x::xs) (y::ys) = (x * y) + innerProduct xs ys;


