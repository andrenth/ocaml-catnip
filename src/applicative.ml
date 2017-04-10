open Prelude

module type I = sig
  type 'a t
  val pure  : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type S = sig
  include I

  val map  : ('a -> 'b) -> 'a t -> 'b t

  module Infix : sig
    val (<$>) : ('a -> 'b)   -> 'a t -> 'b t
    val (>>|) : ('a -> 'b)   -> 'a t -> 'b t
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
    val ( *>) : 'a t -> 'b t -> 'b t
    val (<* ) : 'a t -> 'b t -> 'a t
  end
end

module Make (F : I) : S with type 'a t := 'a F.t = struct
  let pure    = F.pure
  let apply   = F.apply
  let map f a = apply (pure f) a

  module Infix = struct
    let (<$>) = map
    let (>>|) = map
    let (<*>) = apply
    let ( *>) a b = const id <$> a <*> b
    let (<* ) a b = const    <$> a <*> b
  end
end

module type I2 = sig
  type ('a, 'e) t
  val pure  : 'a -> ('a, 'e) t
  val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
end

module type S2 = sig
  include I2

  val map  : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

  module Infix : sig
    val (<$>) : ('a -> 'b)       -> ('a, 'e) t -> ('b, 'e) t
    val (<*>) : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
    val ( *>) : ('a, 'e) t -> ('b, 'e) t -> ('b, 'e) t
    val (<* ) : ('a, 'e) t -> ('b, 'e) t -> ('a, 'e) t
  end
end

module Make2 (F : I2) : S2 with type ('a, 'e) t := ('a, 'e) F.t = struct
  let pure = F.pure
  let apply  = F.apply
  let map f a = apply (pure f) a

  let id a = a
  let const a b = a

  module Infix = struct
    let (<$>) = map
    let (<*>) = apply
    let ( *>) a b = const id <$> a <*> b
    let (<* ) a b = const    <$> a <*> b
  end
end
