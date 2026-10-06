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

let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"

(* a rotor set to a top letter *)
let set wiring turnover top =
  { rotor = { wiring; turnover }; top_letter = top }

(* no rotors, no cables, identity reflector *)
let empty_machine = { refl = identity; rotors = []; plugboard = [] }

(* reflector B, rotors I II III all at A, no cables *)
let machine_AAA =
  {
    refl = refl_B;
    rotors =
      [ set rotor_I 'Q' 'A'; set rotor_II 'E' 'A'; set rotor_III 'V' 'A' ];
    plugboard = [];
  }

let machine_AAA_plug_AG = { machine_AAA with plugboard = [ ('A', 'G') ] }

let cipher_char_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher_char config input) ~printer:(String.make 1)

(* cipher every letter A to Z and compare with the expected 26 letters *)
let cipher_char_all_test name config expected =
  name >:: fun _ ->
    assert_equal expected (String.map (cipher_char config) identity)
      ~printer:(fun s -> s)

let cipher_char_tests =
  [
    cipher_char_all_test "cipher_char empty_machine keeps every letter"
      empty_machine identity;
    cipher_char_test "cipher_char machine_AAA G is P" machine_AAA 'G' 'P';
    cipher_char_test "cipher_char machine_AAA A is U" machine_AAA 'A' 'U';
    cipher_char_test "cipher_char machine_AAA Z is H" machine_AAA 'Z' 'H';
    cipher_char_test "cipher_char machine_AAA P is G" machine_AAA 'P' 'G';
    cipher_char_all_test "cipher_char machine_AAA whole alphabet" machine_AAA
      "UEJOBTPZWCNSRKDGVMLFAQIYXH";
    cipher_char_test "cipher_char machine_AAA_plug_AG A is P"
      machine_AAA_plug_AG 'A' 'P';
    cipher_char_test "cipher_char machine_AAA_plug_AG P is A"
      machine_AAA_plug_AG 'P' 'A';
  ]

(* top letters of all rotors, left to right, as one string *)
let tops config =
  String.concat "" (List.map (fun r -> String.make 1 r.top_letter) config.rotors)

(* rotors I II III with the given top letters, reflector B *)
let machine_I_II_III a b c =
  {
    machine_AAA with
    rotors = [ set rotor_I 'Q' a; set rotor_II 'E' b; set rotor_III 'V' c ];
  }

let one_rotor top = { machine_AAA with rotors = [ set rotor_I 'Q' top ] }

let step_test name config expected =
  name >:: fun _ -> assert_equal expected (tops (step config)) ~printer:(fun s -> s)

(* rule 1: the rightmost rotor always steps *)
let step_rule_1_tests =
  [
    step_test "step one_rotor A is B" (one_rotor 'A') "B";
    step_test "step one_rotor Z is A" (one_rotor 'Z') "A";
    step_test "step empty_machine stays empty" empty_machine "";
    step_test "step I II III AAA is AAB" (machine_I_II_III 'A' 'A' 'A') "AAB";
    step_test "step I II III AAZ is AAA" (machine_I_II_III 'A' 'A' 'Z') "AAA";
    ( "step keeps reflector and plugboard" >:: fun _ ->
      let after = step machine_AAA_plug_AG in
      assert_equal machine_AAA_plug_AG.refl after.refl;
      assert_equal machine_AAA_plug_AG.plugboard after.plugboard );
  ]

(* rotors III II I with the given top letters, reflector B *)
let machine_III_II_I a b c =
  {
    machine_AAA with
    rotors = [ set rotor_III 'V' a; set rotor_II 'E' b; set rotor_I 'Q' c ];
  }

(* top letters after each of n steps *)
let rec run_steps config n =
  if n = 0 then []
  else
    let next = step config in
    tops next :: run_steps next (n - 1)

let step_sequence_test name config expected =
  name >:: fun _ ->
    assert_equal expected
      (run_steps config (List.length expected))
      ~printer:(String.concat " ")

(* rule 2: a rotor at its turnover steps, and so does the one on its left *)
let step_rule_2_tests =
  [
    step_test "step I II III AAQ is AAR" (machine_I_II_III 'A' 'A' 'Q') "AAR";
    step_test "step I II III AAV is ABW" (machine_I_II_III 'A' 'A' 'V') "ABW";
    step_test "step I II III ADA is ADB" (machine_I_II_III 'A' 'D' 'A') "ADB";
    step_test "step I II III AEA is BFB" (machine_I_II_III 'A' 'E' 'A') "BFB";
    step_test "step I II III QAA is QAB" (machine_I_II_III 'Q' 'A' 'A') "QAB";
    step_sequence_test "step III II I KDO gives KDP KDQ KER LFS LFT LFU"
      (machine_III_II_I 'K' 'D' 'O')
      [ "KDP"; "KDQ"; "KER"; "LFS"; "LFT"; "LFU" ];
    step_sequence_test "step III II I VDP gives VDQ VER WFS WFT"
      (machine_III_II_I 'V' 'D' 'P')
      [ "VDQ"; "VER"; "WFS"; "WFT" ];
  ]

(* rule 3: a rotor with two reasons to turn still turns only once *)
let step_rule_3_tests =
  [
    step_test "step one_rotor Q is R" (one_rotor 'Q') "R";
    step_test "step I II III AEV is BFW" (machine_I_II_III 'A' 'E' 'V') "BFW";
    step_test "step I II III QEV is RFW" (machine_I_II_III 'Q' 'E' 'V') "RFW";
  ]

(* all step tests in one list, each rule keeps its own group name *)
let step_tests =
  [
    "rule 1" >::: step_rule_1_tests;
    "rule 2" >::: step_rule_2_tests;
    "rule 3" >::: step_rule_3_tests;
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
           cipher_char_tests;
           step_tests;
         ]

let () = run_test_tt_main suite
