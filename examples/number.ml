module Num  = struct
  type sym =
    | Int of int
    | Point

  let print_alpha = function
    | Int i -> Printf.sprintf "Int(%d)" i
    | Point -> "Point(.)"
end

module DFA = Fsa.Automaton(Num)

let setup () =
  List.iter (* create states *)
    DFA.(fun id -> add_state ~id ())
    [ Nonterminal 0
    ; Accepting 1
    ; Nonterminal 2
    ; Accepting 3
    ];

  (* add all number transitions *)
  for i = 0 to 9 do
    DFA.(add_transition ~from_state:(Nonterminal 0) ~with_sym:(Num.(Int i)) ~to_state:(Accepting 1));
    DFA.(add_transition ~from_state:(Accepting 1)   ~with_sym:(Num.(Int i)) ~to_state:(Accepting 1));
    DFA.(add_transition ~from_state:(Nonterminal 2) ~with_sym:(Num.(Int i)) ~to_state:(Accepting 3))
  done;
  DFA.(add_transition ~from_state:(Accepting 1) ~with_sym:(Num.Point) ~to_state:(Nonterminal 2))

let is_num c = Char.code c > 47 && Char.code c < 58
let is_point c = c='.'

let convert_to_symbol_opt (c : char) : DFA.alphabet option =
  if is_num c then Some Num.(Int ((Char.code c) - 48))
  else if is_point c then Some Num.Point
  else None

let convert_string_to_symbols (str:string) : DFA.alphabet list =
  let len = String.length str in
  let rec conv i =
    if i < len then begin
      match convert_to_symbol_opt str.[i] with
      | Some sym -> sym::(conv (i+1))
      | None     ->      (conv (i+1))
    end else []
  in conv 0

let get_result str : DFA.result =
  DFA.run_through (convert_string_to_symbols str) (Nonterminal 0)

type value =
  | Token of string
  | Invalid of string

let value_string = function
  | Token str -> Printf.sprintf "Token(%s)" str
  | Invalid str -> Printf.sprintf "Invalid(%s)" str

let get_result_token str = match get_result str with
  | Accepted -> Token str
  | Rejected -> Invalid str

let test value =
  try
    get_result_token value
    |> value_string
    |> print_endline
  with
  | DFA.Automaton_failure e -> print_endline (DFA.automaton_error_string e)
  | err -> raise err

let _ =
  setup ();
  List.iter test
    [ "0.9"
    ; "0"
    ; "1.5"
    ; "ishouldfail"
    ; "15.093285"
    ]