open Prelude

module type I = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type S = sig
  include I

  val map   : ('a -> 'b) -> 'a t -> 'b t
  val join  : 'a t t -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
    val (>>)  : 'a t -> 'b t -> 'b t
    val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    val (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
    val (>>|) : ('a -> 'b) -> 'a t -> 'b t
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end
end

module Make (M : I) : S with type 'a t := 'a M.t = struct
  let return  = M.return
  let bind    = M.bind

  let map   f m  = bind (fun a -> M.return (f a)) m
  let join  mm   = bind id mm
  let apply mf m = bind (fun f -> map f m) mf

  module Infix = struct
    let (>>=) m f = M.bind f m
    let (=<<) f m = M.bind f m
    let (>>)  m n = m >>= fun _ -> n
    let (>=>) f g = fun a -> f a >>= g
    let (<=<) g f = fun a -> f a >>= g
    let (<$>) = map
    let (>>|) = map
    let (<*>) = apply
  end
end

module type I2 = sig
  type ('a, 'e) t
  val return : 'a -> ('a, 'e) t
  val bind : ('a -> ('b, 'e) t) -> ('a, 'e) t -> ('b, 'e) t
end

module type S2 = sig
  include I2

  val map   : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  val join  : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val apply : (('a -> 'b), 'e) t -> ('a, 'e) t -> ('b, 'e) t

  module Infix : sig
    val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val (=<<) : ('a -> ('b, 'e) t) -> ('a, 'e) t -> ('b, 'e) t
    val (>>)  : ('a, 'e) t -> ('b, 'e) t -> ('b, 'e) t
    val (>=>) : ('a -> ('b, 'e) t) -> ('b -> ('c, 'e) t) -> 'a -> ('c, 'e) t
    val (<=<) : ('b -> ('c, 'e) t) -> ('a -> ('b, 'e) t) -> 'a -> ('c, 'e) t
    val (<$>) : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    val (>>|) : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    val (<*>) : (('a -> 'b), 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end
end

module Make2 (M : I2) : S2 with type ('a, 'e) t := ('a, 'e) M.t = struct
  let return = M.return
  let bind   = M.bind

  let map   f m  = bind (fun a -> M.return (f a)) m
  let join  mm   = bind id mm
  let apply mf m = bind (fun f -> map f m) mf

  module Infix = struct
    let (>>=) m f = M.bind f m
    let (=<<) f m = M.bind f m
    let (>>)  m n = m >>= fun _ -> n
    let (>=>) f g = fun a -> f a >>= g
    let (<=<) g f = fun a -> f a >>= g
    let (<$>) = map
    let (>>|) = map
    let (<*>) = apply
  end
end

module Free = struct
  module type S = sig
    type 'a u
    type 'a t =
      | Pure   of 'a
      | Free of ('a t) u

    include S with type 'a t := 'a t

    val iter : ('a u -> 'a) -> 'a t -> 'a
    val lift : 'a u -> 'a t
    val map  : ('a -> 'b) -> 'a t -> 'b t
  end

  module Make (F : Functor.I) : S with type 'a u := 'a F.t = struct
    module F = F
    type 'a u = 'a F.t

    module M = struct
      type 'a t =
        | Pure of 'a
        | Free of ('a t) u

      let return a = Pure a

      let rec bind f = function
        | Pure a -> f a
        | Free u -> Free (F.map (bind f) u)
    end

    include M

    let rec iter f = function
      | Pure a -> a
      | Free u -> f (F.map (iter f) u)

    let lift u =
      Free (F.map return u)

    let rec map f = function
      | Pure a -> Pure (f a)
      | Free u -> Free (F.map (map f) u)

    include Make (M)
  end

  module type S2 = sig
    type ('a, 'e) u
    type ('a, 'e) t =
      | Pure   of 'a
      | Free of (('a, 'e) t, 'e) u

    include S2 with type ('a, 'e) t := ('a, 'e) t

    val iter : (('a, 'e) u -> 'a) -> ('a, 'e) t -> 'a
    val lift : ('a, 'e) u -> ('a, 'e) t
    val map  : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  end

  module Make2 (F : Functor.I2) : S2
    with type ('a, 'e) u := ('a, 'e) F.t =
  struct
    module F = F
    type ('a, 'e) u = ('a, 'e) F.t

    module M = struct
      type ('a, 'e) t =
        | Pure of 'a
        | Free of (('a, 'e) t, 'e) u

      let return a = Pure a

      let rec bind f = function
        | Pure a -> f a
        | Free u -> Free (F.map (bind f) u)
    end

    include M

    let rec iter f = function
      | Pure a -> a
      | Free u -> f (F.map (iter f) u)

    let lift u =
      Free (F.map return u)

    let rec map f = function
      | Pure a -> Pure (f a)
      | Free u -> Free (F.map (map f) u)

    include Make2 (M)
  end
end
