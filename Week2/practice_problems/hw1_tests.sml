use "hw1.sml";

val test_is_older01 = is_older((2000,1,1), (2001,1,1)) = true
val test_is_older02 = is_older((2000,1,5), (2000,1,7)) = true
val test_is_older03 = is_older((2000,1,1), (2000,2,1)) = true
val test_is_older04 = is_older((2000,7,5), (2000,8,1)) = true
val test_is_older05 = is_older((2011,3,21), (2011,4,28)) = true
val test_is_older06 = is_older((2011,8,5), (2011,7,21)) = false
val test_is_older07 = is_older((2011,22,8), (2011,21,7)) = false

val jan2000 = (2000,1,1)
val jan2001 = (2001,1,1)
val jan2002 = (2002,1,1)
val feb2000 = (2000,1,2)
val feb2020 = (2020,1,2)
val dec2010 = (2010,1,12)
		  

val threeJanTwoFeb = [jan2000, jan2001, jan2002, feb2000, feb2020, dec2010]
		  
val test_number_in_month01 = number_in_month(threeJanTwoFeb, 1) = 3  
val test_number_in_month02 = number_in_month([], 6) = 0
val test_number_in_month03 = number_in_month(threeJanTwoFeb, 2) = 2
val test_number_in_month04 = number_in_month(threeJanTwoFeb, 3) = 0


val test_number_in_months01 = number_in_months(threeJanTwoFeb, [1,2]) = 5
val test_number_in_months02 = number_in_months(threeJanTwoFeb, [1,3]) = 3
val test_number_in_months03 = number_in_months(threeJanTwoFeb, [2,3]) = 2
val test_number_in_months04 = number_in_months(threeJanTwoFeb, [3,4]) = 0


val dates_in_month01 = dates_in_month(threeJanTwoFeb, 1) = [jan2000, jan2001, jan2002]									    
val dates_in_month02 = dates_in_month(threeJanTwoFeb, 2) = [feb2000, feb2020]
val dates_in_month03 = dates_in_month(threeJanTwoFeb, 3) = []
val dates_in_month04 = dates_in_month(
	[(2,6,2018), (3,7,2019), (1,1,1900)], 7) = [(3,7,2019)]
							       

val dates_in_months01 = dates_in_months(threeJanTwoFeb, [1, 5]) =
			[jan2000,jan2001,jan2002]
val dates_in_months02 = dates_in_months(threeJanTwoFeb, [1, 12]) =
			[jan2000,jan2001,jan2002,dec2010]
val dates_in_months03 = dates_in_months(threeJanTwoFeb, [3, 4, 5]) = []

val string_list = ["one","two","three","four","five","six","seven"]									 
val get_nth_test01 = get_nth(string_list,1) = "one"
val get_nth_test02 = get_nth(string_list,2) = "two"
val get_nth_test03 = get_nth(string_list,5) = "five"
val get_nth_test04 = get_nth(string_list,7) = "seven"


val date_to_string_test01 = date_to_string((1,1,2000)) = "January-1-2000"
val date_to_string_test02 = date_to_string((20,3,2010)) = "March-20-2010"
val date_to_string_test03 = date_to_string((12,12,2012)) = "December-12-2012"

val num_before_reaching_sum_test01 = number_before_reaching_sum(5, [2, 2, 2, 2, 2, 2]) = 2
val num_before_reaching_sum_test02 = number_before_reaching_sum(4, [1, 1, 1, 1, 1, 1]) = 3
val num_before_reaching_sum_test03 = number_before_reaching_sum(10, [2, 2, 2, 2, 2, 2]) = 4
val num_before_reaching_sum_test04 = number_before_reaching_sum(1, [2, 2, 2, 2, 2, 2]) = 0
val num_before_reaching_sum_test05 = number_before_reaching_sum(10, [1, 2, 3, 4, 5]) = 3

val what_month_test01 = what_month(10) = 1
val what_month_test02 = what_month(40) = 2
val what_month_test03 = what_month(70) = 3
val what_month_test04 = what_month(360) = 12
					     
val month_range_test01 = month_range(1,5) = [1,1,1,1,1]
val month_range_test02 = month_range(10,5) = []
val month_range_test03 = month_range(30,32) = [1,1,2]
val month_range_test04 = month_range(31,34) = [1,2,2,2]


val oldest_test01 = oldest([(2,28,2012),(3,31,2011),(4,28,2011)]) = SOME (3,31,2011)
val oldest_test02 = oldest([(1,2,2010)]) = SOME (1,2,2010)
val oldest_test03 = oldest([]) = NONE
val oldest_test04 = oldest([(7,5,1979),(2,4,1985),(6,11,1945)]) = SOME (6,11,1945)


val test_number_in_months_challenge01 = number_in_months_challenge(threeJanTwoFeb, [2,3,2,3]) = 2
											  val test_number_in_months_challenge02 = number_in_months_challenge(threeJanTwoFeb, [1,2,1,2,1]) = 5

											  val dates_in_months_challenge01 = dates_in_months_challenge(threeJanTwoFeb, [1, 5, 1, 5]) = [jan2000,jan2001,jan2002]											    
