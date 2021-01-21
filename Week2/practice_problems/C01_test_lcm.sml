use "C01_is_positive.sml";

(* Least Common Multiple *)
val test_lcm_1 = lcm (18, 12) = 36
val test_lcm_2 = lcm (18, 13) =  234
val test_lcm_3 = lcm (12, 36) = 36
val test_lcm_4 = lcm (12, 15) = 60
val test_lcm_5 = lcm (5, 3) = 15
val test_lcm_6 = lcm (100, 1) = 100
val test_lcm_7 = lcm (1, 10) = 10
