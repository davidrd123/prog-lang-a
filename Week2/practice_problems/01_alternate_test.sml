use "01_alternate.sml";

val test_alternate_1 = alternate [1, 1] = 0
					      
val test_alternate_2 = alternate [1, 2, 3, 4] = ~2
(* 1 -2 +3 -4 -> -2 *)		       

val test_alternate_3 = alternate [] = 0
					  
