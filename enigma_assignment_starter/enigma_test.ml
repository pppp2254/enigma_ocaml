open OUnit2
open Enigma

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input) ~printer:string_of_int

let index_tests =
  [
    index_test "index of A is 0" 'A' 0;
    index_test "index of B is 1" 'B' 1;
    index_test "index of C is 2" 'C' 2;
    index_test "index of M is 12" 'M' 12;
    index_test "index of Z is 25" 'Z' 25;
  ]

let suite = "Enigma test suite" >::: List.flatten [ index_tests ]

let () = run_test_tt_main suite
