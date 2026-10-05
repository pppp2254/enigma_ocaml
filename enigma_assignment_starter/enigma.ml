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

let map_r_to_l wiring top_letter input_pos =
  (* keep n in 0 to 25 even when n is negative *)
  let wrap n = ((n mod 26) + 26) mod 26 in
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  wrap (index wiring.[contact] - offset)

let map_l_to_r wiring top_letter input_pos =
  (* keep n in 0 to 25 even when n is negative *)
  let wrap n = ((n mod 26) + 26) mod 26 in
  let offset = index top_letter in
  let contact = wrap (input_pos + offset) in
  let letter = Char.chr (contact + Char.code 'A') in
  wrap (String.index wiring letter - offset)

let map_refl _wiring _input_pos =
  failwith "Unimplemented"

let map_plug _plugs _c =
  failwith "Unimplemented"

let cipher_char _config _c =
  failwith "Unimplemented"

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
