let check_end word =
  match List.rev word with
  | "AY"::t -> List.rev ("AA"::t)
  | _ -> word

let rec check_before_vo word =
  let voiced_obstruents = ["B"; "D"; "G"; "V"; "DH"; "Z"; "ZH"; "JH"] in
  match word with
  | [] -> []
  | [phon] -> [phon]
  | phon1::phon2::t when (phon1 = "AY" && (List.mem phon2 voiced_obstruents))
    -> "AA"::phon2::(check_before_vo t)
  | phon1::phon2::t -> phon1::(check_before_vo (phon2::t))

(* 1 *)
let general_to_southern word =
  word |> check_end |> check_before_vo

(* 2a *)
let rec sanskrit_to_prakrit1 word =
  match word with
  | [] -> []
  | ["S"] -> ["s"]
  | [phon] -> [phon]
  | "p"::"t"::tl -> "t"::(sanskrit_to_prakrit1 ("t"::tl))
  | "g"::"d"::tl -> "d"::(sanskrit_to_prakrit1 ("d"::tl))
  | "k"::"t"::tl -> "t"::(sanskrit_to_prakrit1 ("t"::tl))
  | "d"::"g"::tl -> "g"::(sanskrit_to_prakrit1 ("g"::tl))
  | "r"::phon2::tl when (phon2 = "d" || phon2 = "k" || phon2 = "p" || phon2 = "m")
    -> phon2::(sanskrit_to_prakrit1 (phon2::tl))
  | "S"::tl -> "s"::(sanskrit_to_prakrit1 tl)
  | phon1::phon2::t -> phon1::(sanskrit_to_prakrit1 (phon2::t))

let rec rules2 word =
  let vowels = ["a";"e";"i";"o";"u"] in
  match word with
  | [] -> []
  | [phon] -> [phon]
  | "a:"::"r"::tl -> "a:"::(rules2 ("l"::tl))
  | v1::"t`"::v2::tl when ((List.mem v1 vowels) && (List.mem v2 vowels)) ->
    rules2 (v1::"d`"::v2::tl)
  | h::t -> h::(rules2 t)

(* 2b *)
let sanskrit_to_prakrit2 word =
  word |> sanskrit_to_prakrit1 |> rules2

let rules3 word =
  let rec rule3a word =
    match List.rev word with
    | "s"::tl -> List.rev tl
    | _ -> word in
  let rec rule3b word =
    let vowels = ["a";"e";"i";"o";"u"] in
    match word with
    | [] -> []
    | v1::"v"::v2::tl when ((List.mem v1 vowels) && (List.mem v2 vowels))
      -> rule3b (v1::"v"::"v"::v2::tl)
    | h::t -> h::(rule3b t) in
  word |> rule3a |> rule3b

(* 2c *)
let sanskrit_to_prakrit3 word =
  word |> sanskrit_to_prakrit2 |> rules3