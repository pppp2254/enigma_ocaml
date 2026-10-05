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

(* rotors are listed left to right but the signal enters on the right,
   so pass through the rest of the list first, then this rotor *)
let rec map_rotors_r_to_l rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest ->
      map_r_to_l r.rotor.wiring r.top_letter (map_rotors_r_to_l rest pos)

(* on the way back the leftmost rotor comes first *)
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

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 1