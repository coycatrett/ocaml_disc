open Maybe
open MaybeOps

let _safeDiv: 'float -> 'float -> 'float option = fun x y -> 
  match y with 
  | 0. -> None
  | y -> Some (x /. y)

let safeDiv x y = let* z = x in y >>= _safeDiv z

let safeHarmonicMean x y = 
  let f1 = Some 1. in
  let* rx = safeDiv f1 x in
  let* ry = safeDiv f1 y in
  safeDiv f1 (Some (rx +. ry))

let () = 
  let result = safeHarmonicMean (Some 10.) (Some 0.01) in 
  match result with
  | Some q -> Printf.printf "Result: %f\n" q
  | None -> Printf.printf "Computation failed\n"