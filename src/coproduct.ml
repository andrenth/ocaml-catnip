module type S = sig
  type 'a left
  type 'a right
  type 'a t =
    | Left  of 'a left
    | Right of 'a right

  include Functor.S with type 'a t := 'a t

  val lift_left  : 'a left  -> 'a t
  val lift_right : 'a right -> 'a t
end

module Make (L : Functor.I) (R : Functor.I) : S
  with type 'a left  := 'a L.t
   and type 'a right := 'a R.t = struct
  module F = struct
    type 'a t =
      | Left  of 'a L.t
      | Right of 'a R.t

    let map f = function
      | Left m  -> Left  (L.map f m)
      | Right m -> Right (R.map f m)
  end

  include F
  include Functor.Make (F)

  let lift_left  a = Left  a
  let lift_right a = Right a
end


