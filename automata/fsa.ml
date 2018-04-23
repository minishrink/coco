
(* -- Abstract machinery used by FSA, PDA, LBA and TM implementations -- *)

module type Alphabet = sig
  type sym
  val print_alpha : sym -> string
end

module Automaton (L : Alphabet) = struct

  (** --- Types for defining state transitions --- **)

  type alphabet = L.sym

  type state_id =
    | Nonterminal of int
    | Accepting   of int
    | Failure     of int

  let print_state = function
    | Nonterminal i -> Printf.sprintf "Nonterminal(%d)" i
    | Accepting i -> Printf.sprintf "Accepting(%d)" i
    | Failure i -> Printf.sprintf "Failure(%d)" i

  type transition_table = (alphabet, state_id) Hashtbl.t

  type state_table = (state_id, transition_table) Hashtbl.t

  type automaton_errors =
    | Unexpected of exn
    | Internal of string
    | State_not_in_db of state_id
    | Transition_already_exists of (state_id * alphabet)
    | No_valid_transition_exists of (state_id * alphabet)

  exception Automaton_failure of automaton_errors
  let automaton_error_string = function
    | Unexpected exn -> raise exn
    | Internal str -> Printf.sprintf "Internal_error(%s)" str
    | State_not_in_db state_id -> Printf.sprintf "State_not_in_db(%s)" (print_state state_id)
    | Transition_already_exists (id, alpha)
      -> Printf.sprintf "Transition_already_exists(%s, %s)" (print_state id) (L.print_alpha alpha)
    | No_valid_transition_exists (id, alpha)
      -> Printf.sprintf "No_valid_transition_exists(%s, %s)" (print_state id) (L.print_alpha alpha)

  (** -n-- Mutable values --- **)

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
    then raise (Automaton_failure (State_not_in_db state))

  (* For now, raise an exception to enforce determinism *)
  let add_transition ~(from_state : state_id) ~(with_sym : alphabet) ~(to_state : state_id) =
    assert_in_db from_state;
    assert_in_db to_state;
    let transitions = Hashtbl.find state_db from_state in
    if Hashtbl.mem transitions with_sym
    then raise (Automaton_failure (Transition_already_exists (from_state, with_sym)))
    else Hashtbl.add transitions with_sym to_state

  let get_state_transitions_with_id id =
    try
      Hashtbl.find state_db id
    with Not_found -> raise (Automaton_failure (State_not_in_db id))

  let get_state_from_transition (table : transition_table) with_sym =
    try
      Hashtbl.find table with_sym
    with
    | Automaton_failure(e) as err -> raise err
    | exn -> raise exn

  let get_next_state ~from ~with_sym =
    try
      let transitions = get_state_transitions_with_id from in
      get_state_from_transition transitions with_sym
    with
    | Not_found -> raise (Automaton_failure (No_valid_transition_exists (from, with_sym)))
    | Automaton_failure(e) as err -> raise err

  type result = Accepted | Rejected
  let result_string = function
    | Accepted -> "ACCEPTED"
    | Rejected -> "REJECTION"

  let run_through (input : alphabet list) (from_state : state_id) =
    let rec run (from : state_id) = function
      | a::b ->
        begin match (get_next_state ~from ~with_sym:a) with
          | Failure _ -> Rejected
          | (Nonterminal _) as state -> run state b
          | Accepting _ -> Accepted
        end
      | [] ->
        begin match from with
          | Accepting _ -> Accepted
          | (Nonterminal _ | Failure _) -> Rejected
        end
    in
    run from_state input
end
