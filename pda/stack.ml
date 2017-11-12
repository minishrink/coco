type 'a stack = Stk of 'a list
exception EmptyStack

let make_stack lst = Stk lst

let push x = function
  | Stk lst -> Stk (x::lst)

let stkHd = function
  | Stk a::b -> a
  | _ -> raise EmptyStack

let pop = function
  | Stk (a::b) -> Stk b
  | _ -> raise EmptyStack

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
    in lst |> enq x |> make_queue

let dequeue = function
  | Que lst ->
    let deq = function
      | a::b -> b
      | _ -> raise EmptyQueue
    in lst |> deq |> make_queue
