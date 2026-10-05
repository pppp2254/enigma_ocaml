open OUnit2
open Enigma

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input) ~printer:string_of_int

let index_tests =
  [
    index_test "index A is 0" 'A' 0;
    index_test "index B is 1" 'B' 1;
    index_test "index C is 2" 'C' 2;
    index_test "index M is 12" 'M' 12;
    index_test "index Z is 25" 'Z' 25;
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
    map_r_to_l_test "map_r_to_l identity A 0 is 0" identity 'A' 0 0;
    map_r_to_l_test "map_r_to_l identity C 5 is 5" identity 'C' 5 5;
    map_r_to_l_test "map_r_to_l swap_ab A 0 is 1" swap_ab 'A' 0 1;
    map_r_to_l_test "map_r_to_l swap_ab A 1 is 0" swap_ab 'A' 1 0;
    map_r_to_l_test "map_r_to_l swap_ab A 2 is 2" swap_ab 'A' 2 2;
    map_r_to_l_test "map_r_to_l swap_ab B 0 is 25" swap_ab 'B' 0 25;
    map_r_to_l_test "map_r_to_l swap_ab B 25 is 0" swap_ab 'B' 25 0;
    map_r_to_l_test "map_r_to_l swap_ab C 0 is 0" swap_ab 'C' 0 0;
    map_r_to_l_test "map_r_to_l rotor_I A 0 is 4" rotor_I 'A' 0 4;
    map_r_to_l_test "map_r_to_l rotor_I B 0 is 9" rotor_I 'B' 0 9;
    map_r_to_l_test "map_r_to_l rotor_III O 14 is 17" rotor_III 'O' 14 17;
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
    map_l_to_r_test "map_l_to_r identity A 0 is 0" identity 'A' 0 0;
    map_l_to_r_test "map_l_to_r swap_ab A 0 is 1" swap_ab 'A' 0 1;
    map_l_to_r_test "map_l_to_r swap_ab B 0 is 25" swap_ab 'B' 0 25;
    map_l_to_r_test "map_l_to_r swap_ab C 0 is 0" swap_ab 'C' 0 0;
    map_l_to_r_test "map_l_to_r rotor_I A 0 is 20" rotor_I 'A' 0 20;
    map_l_to_r_test "map_l_to_r rotor_I F 10 is 14" rotor_I 'F' 10 14;
    round_trip_test "map_l_to_r undoes map_r_to_l for rotor_I B" rotor_I 'B';
    round_trip_test "map_l_to_r undoes map_r_to_l for rotor_III O" rotor_III 'O';
  ]

let refl_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let refl_C = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

let map_refl_test name wiring input expected =
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input) ~printer:string_of_int

(* reflecting twice gives the start position *)
let reflect_twice_test name wiring =
  name >:: fun _ ->
    let all = List.init 26 (fun i -> i) in
    let back = List.map (fun i -> map_refl wiring (map_refl wiring i)) all in
    assert_equal all back

let map_refl_tests =
  [
    map_refl_test "map_refl identity 0 is 0" identity 0 0;
    map_refl_test "map_refl identity 1 is 1" identity 1 1;
    map_refl_test "map_refl identity 25 is 25" identity 25 25;
    map_refl_test "map_refl refl_B 0 is 24" refl_B 0 24;
    map_refl_test "map_refl refl_B 24 is 0" refl_B 24 0;
    map_refl_test "map_refl refl_C 0 is 5" refl_C 0 5;
    map_refl_test "map_refl refl_C 5 is 0" refl_C 5 0;
    reflect_twice_test "map_refl twice gives start for refl_B" refl_B;
    reflect_twice_test "map_refl twice gives start for refl_C" refl_C;
  ]

let one_pair = [ ('A', 'Z') ]
let two_pairs = [ ('A', 'Z'); ('X', 'Y') ]
let two_pairs_flipped = [ ('Y', 'X'); ('Z', 'A') ]

let map_plug_test name plugs input expected =
  name >:: fun _ ->
    assert_equal expected (map_plug plugs input) ~printer:(String.make 1)

let map_plug_tests =
  [
    map_plug_test "map_plug empty A is A" [] 'A' 'A';
    map_plug_test "map_plug empty Z is Z" [] 'Z' 'Z';
    map_plug_test "map_plug one_pair A is Z" one_pair 'A' 'Z';
    map_plug_test "map_plug one_pair Z is A" one_pair 'Z' 'A';
    map_plug_test "map_plug one_pair B is B" one_pair 'B' 'B';
    map_plug_test "map_plug two_pairs A is Z" two_pairs 'A' 'Z';
    map_plug_test "map_plug two_pairs Z is A" two_pairs 'Z' 'A';
    map_plug_test "map_plug two_pairs X is Y" two_pairs 'X' 'Y';
    map_plug_test "map_plug two_pairs Y is X" two_pairs 'Y' 'X';
    map_plug_test "map_plug two_pairs M is M" two_pairs 'M' 'M';
    map_plug_test "map_plug two_pairs_flipped A is Z" two_pairs_flipped 'A' 'Z';
    map_plug_test "map_plug two_pairs_flipped X is Y" two_pairs_flipped 'X' 'Y';
  ]

let suite =
  "Enigma test suite"
  >::: List.flatten
         [
           index_tests;
           map_r_to_l_tests;
           map_l_to_r_tests;
           map_refl_tests;
           map_plug_tests;
         ]

let () = run_test_tt_main suite
