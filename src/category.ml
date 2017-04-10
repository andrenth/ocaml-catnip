module type I = sig
  type ('a, 'b) t
  val id      : ('a, 'a) t
  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end

module type S = sig
  include I
  module Infix : sig
    val (%)   : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
    val (<<<) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
    val (>>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  end
end

module Make (C : I) : S with type ('a, 'b) t := ('a, 'b) C.t = struct
  include C

  module Infix = struct
    let (%)       = compose
    let (<<<)     = compose
    let (>>>) f g = compose g f
  end
end
