(*
TODO Figure out the dune config so that I can include my Lazy module
*)
module Loop = struct
  type ('state, 'output) loop = 
    | Done of 'output
    | Continue of 'state

  let rec while_: 'state -> ('state -> ('state, 'output) loop) -> 'output = fun state body -> 
    match body state with
    | Done output -> output
    | Continue state -> while_ state body
end