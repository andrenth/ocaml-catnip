open Prelude

module type I = sig
  type ('a, 'b) t
  val bimap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module type S = sig
  include I
  val first : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
end

module Make (B : I) : S with type ('a, 'b) t := ('a, 'b) B.t = struct
  include B

  let first  f = B.bimap f id
  let second f = B.bimap id f
end
