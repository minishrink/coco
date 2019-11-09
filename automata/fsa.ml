
(* -- Abstract machinery used by FSA, PDA, LBA and TM implementations -- *)

module type Alphabet = sig
  type sym
  val print_alpha : sym -> string
  val convert_to_symbol_opt : char -> sym option

  type token
  type token_flag
  val token_of_flag : token_flag -> string -> token
  val token_flag_string : token_flag -> string
  val token_string : token -> string
end

module Automaton (L : Alphabet) = struct

  (** --- Types for defining state transitions --- **)

  type alphabet = L.sym

  type state_id =
    | Nonterminal of int
    | Accepting   of (int * L.token_flag)
    | Failure     of int

  let print_state = function
    | Nonterminal i      -> Printf.sprintf "Nonterminal(%d)" i
    | Accepting (i,flag) -> Printf.sprintf "Accepting(%d : %s)" i (L.token_flag_string flag)
    | Failure i          -> Printf.sprintf "Failure(%d)" i

  type automaton_errors =
    | Unexpected of exn
    | Internal of string
    | State_not_in_db of state_id
    | Transition_already_exists of (state_id * alphabet)
    | No_valid_transition_exists of (state_id * alphabet)

  let automaton_error_string = function
    | Unexpected exn -> raise exn
    | Internal str
      -> Printf.sprintf "Internal_error(%s)" str
    | State_not_in_db state_id ->
      Printf.sprintf "State_not_in_db(%s)" (print_state state_id)
    | Transition_already_exists (id, alpha) ->
      Printf.sprintf "Transition_already_exists(%s, %s)" (print_state id) (L.print_alpha alpha)
    | No_valid_transition_exists (id, alpha) ->
      Printf.sprintf "No_valid_transition_exists(%s, %s)" (print_state id) (L.print_alpha alpha)

  exception Automaton_failure of automaton_errors
  let fsa_exn err = raise (Automaton_failure err)

  (** --- Mutable values --- **)

  type transition_table = (alphabet, state_id) Hashtbl.t
  type state_table = (state_id, transition_table) Hashtbl.t
  let state_db : state_table = Hashtbl.create 10

  (** --- Helper functions --- **)

  (* DETERMINISM FOR NOW *)
  let add_state ~(id : state_id) ?(transitions : transition_table = Hashtbl.create 5) () =
    (* can only have one entry per state at any time *)
    if Hashtbl.mem state_db id
    then Hashtbl.replace state_db id transitions
    else Hashtbl.add state_db id transitions

  let assert_in_db state =
    if not (Hashtbl.mem state_db state)
    then fsa_exn (State_not_in_db state)

  (* For now, raise an exception to enforce determinism *)
  let add_transition ~(from_state : state_id) ~(with_sym : alphabet) ~(to_state : state_id) =
    assert_in_db from_state;
    assert_in_db to_state;
    let transitions = Hashtbl.find state_db from_state in
    if Hashtbl.mem transitions with_sym
    then fsa_exn (Transition_already_exists (from_state, with_sym))
    else Hashtbl.add transitions with_sym to_state


  (** State transition logic **)

  let get_state_transitions_with_id id =
    try
      Hashtbl.find state_db id
    with Not_found -> fsa_exn (State_not_in_db id)

  let get_state_from_transition (table : transition_table) with_sym =
    try
      Hashtbl.find table with_sym
    with
    | Automaton_failure(_) as err -> raise err
    | exn -> raise exn

  let get_next_state ~from ~with_sym =
    try
      let transitions = get_state_transitions_with_id from in
      get_state_from_transition transitions with_sym
    with
    | Not_found -> fsa_exn (No_valid_transition_exists (from, with_sym))
    | Automaton_failure(_) as err -> raise err

  (** Result printing and interpreting logic **)

  type result =
    | Accepted of L.token_flag (* indicate token type of accepting state *)
    | Rejected

  let result_string = function
    | Accepted _ -> "[ ACCEPTED ]"
    | Rejected   -> "[ REJECTED ]"

  let convert_string_to_symbols str : alphabet list =
    let len = String.length str in
    let rec conv i =
      if i < len then begin
        match L.convert_to_symbol_opt str.[i] with
        | Some sym -> sym::(conv (i+1))
        | None     ->      (conv (i+1))
      end else []
    in conv 0

  let run_through (input : alphabet list) (from_state : state_id) =
    let rec run (from : state_id) = function
      | a::b ->
        begin match (get_next_state ~from ~with_sym:a) with
          | Failure _ -> Rejected
          | Nonterminal _
          (* TODO does this work? *)
          | Accepting _ as state -> run state b
        end
      | [] ->
        begin match from with
          |  Accepting (_, flag)        -> Accepted flag
          | (Nonterminal _ | Failure _) -> Rejected
        end
    in
    run from_state input

  let get_result str : result =
    run_through
      (convert_string_to_symbols str)
      (Nonterminal 0)

  type value =
    | Token of L.token
    | Invalid of string

  let value_string = function
    | Token tkn -> Printf.sprintf "Token(%s)" L.(token_string tkn)
    | Invalid str -> Printf.sprintf "Invalid(%s)" str

  let value_of_result str = match get_result str with
    | Accepted flag -> Token L.(token_of_flag flag str)
    | Rejected -> Invalid str

  let lex_token str = match value_of_result str with
    | Token tkn -> tkn
    | Invalid _ -> failwith "not a token"
end
