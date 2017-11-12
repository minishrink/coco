type 'a stack = Lst of 'a list
exception EmptyStack

let make_stack lst = Lst lst

let push x = function
  | Lst lst -> Lst (x::lst)

let stkHd = function
  | Lst a::b -> a
  | _ -> raise EmptyStack

let pop = function
  | Lst (a::b) -> Lst b
  | _ -> raise EmptyStack

let example make_struct add rm () =
  []
  |> make_struct
  |> add "blob"
  |> add "floop"
  |> rm
  |> add "whadup"
  |> rm
  |> rm

(****************************
 *********  QUEUES  *********
 ****************************)

type 'a queue = Que of 'a list
exception EmptyQueue

let make_queue lst = Que lst

let enqueue x = function
  | Que lst ->
    let rec enq x lst =
      match lst with
      | [] -> [x]
      | a::b -> a::(enq x b)
    in Que (enq x lst)

(* FIXME *)
let dequeue lst =
  match lst with
  | Que lst ->
    let deq = function
      | a::b -> b
      | _ -> raise EmptyQueue
    in Que (deq lst)

