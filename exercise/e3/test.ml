open E3
open OUnit2


let make_product_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (lst_product input) ~printer: string_of_int)


let make_fifth_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (fifth input) ~printer: string_of_int)

let make_descend_list_test name expected_output input =
    name >:: (fun _ -> assert_equal ~cmp: (List.equal Int.equal) expected_output (descend_list input))

let tests = "test suite for product" >::: [
  make_product_test "empty" 1 [];
  make_product_test "singleton" 1 [1];
  make_product_test "two_elements" 2 [1; 2];
]

let test_for_fifth = "test suite for fifth" >::: [
  make_fifth_test "empty" 0 [];
  make_fifth_test "singleton" 0 [1];
  make_fifth_test "five_elements" 5 [1;2;3;4;5];
  make_fifth_test "six_elements" 5 [1;2;3;4;5;6];
]

let test_for_descend_list = "test suite for descend_list" >::: [
  make_descend_list_test "empty" [] [];
  make_descend_list_test "singleton" [1] [1];
  make_descend_list_test "ascend_elements" [5;4;3;2;1] [1;2;3;4;5];
  make_descend_list_test "random_elements" [5;4;3;2;1] [4;1;2;3;5];
]

let test_for_list_max = "test suite for list max" >::: [
  "empty" >:: (fun _ -> assert_raises (Failure "list_max") (fun () -> list_max []));
  "singleton" >:: (fun _ -> assert_equal 1 (list_max [1]));
  "two_elements" >:: (fun _ -> assert_equal 2 (list_max [1; 2]));
  "many_elements" >:: (fun _ -> assert_equal 10086 (list_max [1; 10086; 2; 4; -1]))
]

let _ = run_test_tt_main tests

let _ = run_test_tt_main test_for_fifth

let _ = run_test_tt_main test_for_descend_list

let _ = run_test_tt_main test_for_list_max