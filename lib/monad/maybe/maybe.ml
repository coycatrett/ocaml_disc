open Monad

module Maybe = struct
  type 'a t = 'a option

  let pure x = Some x

  let bind m f = 
    match m with
    | None -> None
    | Some x -> f x
end

module MaybeOps = MonadOps(Maybe)