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

let identity = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let swap_ab = "BACDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let map_r_to_l_test name wiring top input expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top input) ~printer:string_of_int

let map_r_to_l_tests =
  [
    map_r_to_l_test "r to l identity A 0" identity 'A' 0 0;
    map_r_to_l_test "r to l identity C 5" identity 'C' 5 5;
    map_r_to_l_test "r to l swap_ab A 0" swap_ab 'A' 0 1;
    map_r_to_l_test "r to l swap_ab A 1" swap_ab 'A' 1 0;
    map_r_to_l_test "r to l swap_ab A 2" swap_ab 'A' 2 2;
    map_r_to_l_test "r to l swap_ab B 0 wraps below 0" swap_ab 'B' 0 25;
    map_r_to_l_test "r to l swap_ab B 25 wraps above 25" swap_ab 'B' 25 0;
    map_r_to_l_test "r to l swap_ab C 0" swap_ab 'C' 0 0;
    map_r_to_l_test "r to l rotor I A 0" rotor_I 'A' 0 4;
    map_r_to_l_test "r to l rotor I B 0" rotor_I 'B' 0 9;
    map_r_to_l_test "r to l rotor III O 14" rotor_III 'O' 14 17;
  ]

let map_l_to_r_test name wiring top input expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top input) ~printer:string_of_int

(* going right to left and back again gives the start position *)
let round_trip_test name wiring top =
  name >:: fun _ ->
    let all = List.init 26 (fun i -> i) in
    let back =
      List.map (fun i -> map_l_to_r wiring top (map_r_to_l wiring top i)) all
    in
    assert_equal all back

let map_l_to_r_tests =
  [
    map_l_to_r_test "l to r identity A 0" identity 'A' 0 0;
    map_l_to_r_test "l to r swap_ab A 0" swap_ab 'A' 0 1;
    map_l_to_r_test "l to r swap_ab B 0 wraps below 0" swap_ab 'B' 0 25;
    map_l_to_r_test "l to r swap_ab C 0" swap_ab 'C' 0 0;
    map_l_to_r_test "l to r rotor I A 0" rotor_I 'A' 0 20;
    map_l_to_r_test "l to r rotor I F 10" rotor_I 'F' 10 14;
    round_trip_test "round trip rotor I top B" rotor_I 'B';
    round_trip_test "round trip rotor III top O" rotor_III 'O';
  ]

let suite =
  "Enigma test suite"
  >::: List.flatten [ index_tests; map_r_to_l_tests; map_l_to_r_tests ]

let () = run_test_tt_main suite
