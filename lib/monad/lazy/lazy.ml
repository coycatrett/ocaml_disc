module Lazy = struct
  type 'a cache = Val of 'a | Func of (unit -> 'a)
  type 'a t = 'a cache ref

  (** Forces the evaluation of a lazy type *)
  let force : 'a t -> 'a = fun x -> 
    match !x with 
    | Val v -> v
    | Func l -> 
      let lazy_val = l () in  (* only evaluation the expression once *)
      x := Val lazy_val; lazy_val  (* update the cached value and return *)
    
  (* A Thunk is like and unfinished thought *)
  (* hiding implementation details of our representation of lazy value *)
  (** Returns the 'a cache ref corresponding to the 'a expression *)
  let fromThunk : (unit -> 'a) -> 'a t = fun u -> ref (Func u)

  (** Takes a value and converts ~it to a cache ref *)
  let pure : 'a -> 'a t = print_endline "pure"; fun x -> ref (Val x)

  let bind : 'a t -> ('a -> 'b t) -> 'b t = fun x f -> fromThunk (fun () -> force(f (force x)))
end

module Loop = struct
  type ('state, 'output) loop = 
    | Done of 'output
    | Continue of 'state

  let rec while_: 'state -> ('state -> ('state, 'output) loop) -> 'output = fun state body -> 
    match body state with
    | Done output -> output
    | Continue state -> while_ state body
  
  open Lazy
  let rec while_lazy_not_safe : 'state -> ('state -> ('state,'output) loop Lazy.t) -> 'output Lazy.t = fun state body -> 
    fromThunk (fun () -> 
      match force (body state) with
      | Done output -> output
      | Continue state -> force (while_lazy_not_safe state body)
      (* 
        not recursive, only self referencing. 
        trampolines back and forth between while_lazy and force, adding a stack frame each time
      *)
    )

    (** Tests if a function is statck safe *)
    let with_stack_limit stack_limit f =
      let old_gc_settings = Gc.get () in
      Gc.set { old_gc_settings with stack_limit };
      Fun.protect ~finally:(fun () -> Gc.set old_gc_settings) f

  let while_lazy : 'state -> ('state -> ('state,'output) loop Lazy.t) -> 'output Lazy.t = fun state body -> 
    fromThunk (fun () -> 
      let rec f state body = 
      match force (body state) with
      | Done output -> output
      | Continue state -> f state body  (* tail recursive *)
    in f state body) 
end


