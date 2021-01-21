fun is_positive (x : int) =
    x > 0
	      
fun is_divisible_by (x : int, y : int) =
    x mod y = 0;

fun divide_by (a : int, b : int) =
    if a < b
    then 0
    else 1 + divide_by(a - b, b) 

fun gcd (a : int, b : int) =
    if b = 0
    then a
    else gcd(b, a mod b)

fun lcm (a : int, b : int) =
    (a * b) div gcd (a, b)


fun gcd_list (xs : int list) =
    if null (tl xs)
    then hd xs
    else gcd (hd xs, gcd_list(tl xs))
