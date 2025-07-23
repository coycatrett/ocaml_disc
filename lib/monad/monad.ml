(*
  In category theory, a monad over a category C is described as a triple (T, 𝜂, 𝝁) where 

   - T : C -> C is an endofunctor
   - 𝜂 : 1_C => T is a natural transformation called the unit
   - 𝝁 : TT => T is a natural transformation called the multiplication

   subject to

   - The Unit Law: 𝝁 . T𝜂 = 1_T = 𝝁 . 𝜂T
   - The Associativity Law: 𝝁 . T𝝁 = 𝝁 . 𝝁T
 *)

 (*
  An Extension System (https://ncatlab.org/nlab/show/extension+system) on a category C consists of 
   - For every object c in C, an object Tc and a morphism 𝜂_c : c -> Tc
   - For every morphism f : a -> Tb in C, a morphism f* : Ta -> Tb (the Kleisli extension of f) satisfying the following:
     - For every a in C, (𝜂_a)* = id_{Ta}
     - For every f : a -> Tb, f* o 𝜂_a = f
     - For every f : a -> Tb and g : b -> Tc, g* o f* = (g* o f)*
   
  In functional programming, to specify a monad, we specify an extension system.
 *)

 (*
  Given an extension system (T, 𝜂, * ),
   - turn T into a functor by putting Tf = (𝜂_b o f)* for f : a -> b
   - 𝜂 : 1_C => T will be natural
   - Put 𝝁_a = (id_{Ta})* : TTa -> Ta for a in C
  and so (T, 𝜂, 𝝁) is the monad arising from the extension system (T, 𝜂, * ).

  Given a monad (T, 𝜂, 𝝁),
   - For every object c in C, Tc is an object and 𝜂_c : c -> Tc is a morphism
   - For every morphism f : a -> Tb in C, f* = 𝝁_b o Tf : Ta -> TTb -> Tb is such that:
    - For every a in C, (𝜂_a)* = 𝝁_a o T𝜂_a = id_{Ta} by the Unit Law
    - For every f : a -> Tb in C, f* o 𝜂_a = f by naturality of 𝜂 and the Unit Law
    - For every f : a -> Tb and g : b -> Tc, g* o f* = (g* o f)* by naturality and the Associative Law
  and so (T, 𝜂, * ), is the extension system arising from the monad (T, 𝜂, 𝝁).


  The extension system arising from the monad of an extension system is the original extension system
  The monad arisng from the extension system of a monad is the orignal monad
  This gives a natural? bijection between extension systems on C and monads over C
 *)


module type Monad = sig 
  (* Object function of T *)
  type 'a t

  (* pure is exactly 𝜂 *)
  val pure : 'a -> 'a t

  (* Morphism function of U : C_T -> C where C_T is the Kleisli category of T *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  
  (* TODO
    The three monad laws:
  *)
end


(** A monad module with some nice functionality *)
module MonadOps (M : Monad) = struct
  open M

  let (>>=) = bind

  (** 
    Syntactic sugar:  
    let* x = a in e <=> bind a (fun x -> e) 
  *)
  let (let*) = bind

  let uncurr_bind : ('a -> 'b t) -> ('a t -> 'b t) = fun f -> Fun.flip bind f

  (* Morphism function of T, T_Mor *)
  let map : 'a t -> ('a -> 'b) -> 'b t = fun m f -> bind m (Fun.compose pure f)
  (* Alternatively, Fun.compose pure (Fun.flip bind f) m *)

  let uncurr_map : ('a -> 'b) -> ('a t -> 'b t) = fun f -> uncurr_bind (Fun.compose pure f)

  (* mu_a *)
  let join : 'a t t -> 'a t = fun mm -> bind mm (fun m -> m)
    (* Fun.flip bind (fun m -> m) 
    gives error 'a t t -> _a t ???
    *)

  let compose : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t) = fun f g -> fun x -> bind (f x) g
  let (>=>) = compose

  (* TODO: do notation. ask patrick *)
  (* TODO: lift2 *)
end


(* The Kleisli Category of a Monad *)
module Kleisli (M : Monad) = struct
  open M

  type ('a, 'b) k_arr = 'a -> 'b t

  let id = pure

  (* Kleisli composition *)
  let compose : ('a, 'b) k_arr -> ('b, 'c) k_arr -> ('a, 'c) k_arr = fun f g -> fun x -> bind (f x) g

  let (>=>) = compose
end