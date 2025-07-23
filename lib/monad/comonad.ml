module type CoMonad_ = sig
  type 'a t
  val extract : 'a t -> 'a
  val dup : 'a t -> ('a t -> 'b) -> 'b t
end

module CoMonad (C : CoMonad_) = struct
  
end