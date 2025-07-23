module Lazy = struct
  type 'a t = Val | Func of (unit -> 'a)
end