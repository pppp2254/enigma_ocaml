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

(** [wrap n] wraps [n] into 0-25 range safely for negative numbers. *)
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

(* A reflector acts as a stationary rotor *)
let map_refl wiring input_pos = map_r_to_l wiring 'A' input_pos

(* Swaps c if connected to a plugboard pair *)
let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (one_end, other_end) :: rest ->
      if c = one_end then other_end
      else if c = other_end then one_end
      else map_plug rest c

(** [map_rotors_r_to_l rotors input_pos] passes signal right-to-left. *)
let rec map_rotors_r_to_l rotors input_pos =
  match rotors with
  | [] -> input_pos
  | r :: rest ->
      map_r_to_l r.rotor.wiring r.top_letter (map_rotors_r_to_l rest input_pos)

(** [map_rotors_l_to_r rotors input_pos] passes signal left-to-right. *)
let rec map_rotors_l_to_r rotors input_pos =
  match rotors with
  | [] -> input_pos
  | r :: rest ->
      map_rotors_l_to_r rest (map_l_to_r r.rotor.wiring r.top_letter input_pos)

(* Signal path: plugboard -> rotors -> reflector -> rotors -> plugboard *)
let cipher_char config c =
  c
  |> map_plug config.plugboard
  |> index
  |> map_rotors_r_to_l config.rotors
  |> map_refl config.refl
  |> map_rotors_l_to_r config.rotors
  |> ( + ) (Char.code 'A')
  |> Char.chr
  |> map_plug config.plugboard

(** [advance r] moves top letter forward by one, wrapping 'Z' to 'A'. *)
let advance r =
  let next_pos = wrap (index r.top_letter + 1) in
  { r with top_letter = Char.chr (next_pos + Char.code 'A') }

(** [at_turnover r] is true when top letter equals turnover letter. *)
let at_turnover r = r.top_letter = r.rotor.turnover

(** [step_rotors is_leftmost rotors] steps [rotors] (left to right).
    A rotor turns if it is rightmost, if the rotor on its right is at
    turnover, or if it is at its own turnover and not leftmost. *)
let rec step_rotors is_leftmost rotors =
  match rotors with
  | [] -> []
  | [ r ] -> [ advance r ]
  | r :: (right :: _ as rest) ->
      let turns = at_turnover right || (at_turnover r && not is_leftmost) in
      (if turns then advance r else r) :: step_rotors false rest

(* Creates new config with stepped rotors *)
let step config = { config with rotors = step_rotors true config.rotors }

(** [cipher_list config chars] steps machine then ciphers each character. *)
let rec cipher_list config chars =
  match chars with
  | [] -> []
  | c :: rest ->
      let stepped = step config in
      cipher_char stepped c :: cipher_list stepped rest

(* Converts string to list, ciphers, and converts back to string *)
let cipher config message =
  message
  |> String.to_seq
  |> List.of_seq
  |> cipher_list config
  |> List.to_seq
  |> String.of_seq

let hours_worked = 4
