module Num  = struct
  type sym =
    | Int of int
    | Point

  type token =
    | Int of string
    | Float of string

  type token_flag =
    | Int | Float

  let token_of_flag (flag : token_flag) input : token =
    match flag with
    | Int   -> Int input
    | Float -> Float input

  let token_flag_string = function
    | Int -> "INT" | Float -> "FLOAT"

  let token_string : token -> string = function
    | Int i   -> Printf.sprintf "Int(%s)" i
    | Float f -> Printf.sprintf "Float(%s)" f

  let print_alpha : sym -> string = function
    | Int i -> Printf.sprintf "Int(%d)" i
    | Point -> "Point(.)"

  let convert_to_symbol_opt (c : char) : sym option =
    let is_num c = Char.code c > 47 && Char.code c < 58 in
    let is_point c = c='.' in

    if is_num c then Some (Int ((Char.code c) - 48))
    else if is_point c then Some Point
    else None
end

module DFA = Fsa.Automaton(Num)

let setup () =
  List.iter (* create states *)
    DFA.(fun id -> add_state ~id ())
    [ Nonterminal 0
    ; Accepting  (1, Num.Int)
    ; Nonterminal 2
    ; Accepting  (3, Num.Float)
    ];

  (* add all number transitions *)
  for i = 0 to 9 do
    DFA.(add_transition ~from_state:(Nonterminal 0) ~with_sym:Num.(Int i) ~to_state:(Accepting (1, Num.Int)));
    DFA.(add_transition ~from_state:(Nonterminal 2) ~with_sym:Num.(Int i) ~to_state:(Accepting (3, Num.Float)));
    DFA.(add_transition ~from_state:(Accepting (1, Num.Int)) ~with_sym:Num.(Int i) ~to_state:(Accepting (1, Num.Int)))
  done;
  DFA.(add_transition ~from_state:(Accepting (1,Num.Int)) ~with_sym:Num.Point ~to_state:(Nonterminal 2))

