module type I = sig
  type 'a t
  val empty  : 'a
  val append : 'a -> 'a -> 'a
end

module type S = sig
  include I
  val concat : 'a list -> 'a

  module Infix : sig
    val (<<>>) : 'a -> 'a -> 'a
  end
end

module Make (M : I) = struct
  include M

  let concat ms = List.fold_left append empty ms

  module Infix = struct
    let (<<>>) = append
  end
end
