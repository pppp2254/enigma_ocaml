type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

let index c = Char.code c - Char.code 'A'

(* keep n in 0 to 25 even when n is negative *)
let wrap n = ((n mod 26) + 26) mod 26

let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  wrap (index wiring.[contact] - offset)

let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  let letter = Char.chr (contact + Char.code 'A') in
  wrap (String.index wiring letter - offset)

(* a reflector works like a rotor that never turns *)
let map_refl wiring input_pos = map_r_to_l wiring 'A' input_pos

(* look through each wire, swap c if it is on one end *)
let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (a, b) :: rest ->
      if c = a then b
      else if c = b then a
      else map_plug rest c

(* signal enters on the right *)
let rec map_rotors_r_to_l rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      map_r_to_l r.rotor.wiring r.top_letter (map_rotors_r_to_l rest pos)

(* signal enters on the left *)
let rec map_rotors_l_to_r rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      map_rotors_l_to_r rest (map_l_to_r r.rotor.wiring r.top_letter pos)

let cipher_char config c =
  let start = index (map_plug config.plugboard c) in
  let at_reflector = map_rotors_r_to_l config.rotors start in
  let back = map_refl config.refl at_reflector in
  let finish = map_rotors_l_to_r config.rotors back in
  map_plug config.plugboard (Char.chr (finish + Char.code 'A'))

(* turn a rotor by one letter, Z goes back to A *)
let advance r =
  let next = wrap (index r.top_letter + 1) in
  { r with top_letter = Char.chr (next + Char.code 'A') }

let at_turnover r = r.top_letter = r.rotor.turnover

(* Stepping rules from left to right:
   1. Rightmost always steps.
   2. Steps if the rotor to its right is at turnover.
   3. Double-stepping: steps if at its own turnover except leftmost. *)
let rec step_rotors is_leftmost rotors =
  match rotors with
  | [] -> []
  | [ r ] -> [ advance r ]
  | r :: (right :: _ as rest) ->
      let turns = at_turnover right || (at_turnover r && not is_leftmost) in
      (if turns then advance r else r) :: step_rotors false rest

let step config = { config with rotors = step_rotors true config.rotors }

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 1