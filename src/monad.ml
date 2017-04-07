let (%) f g x = f (g x)

module type I = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type S = sig
  include I
  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>)  : 'a t -> 'b t -> 'b t
    val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    val (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  end
end

module Make (M : I) : S with type 'a t := 'a M.t = struct
  let return = M.return
  let bind   = M.bind

  module Infix = struct
    let (>>=) m f = M.bind f m
    let (>>)  m n = m >>= fun _ -> n
    let (>=>) f g = fun a -> f a >>= g
    let (<=<) g f = fun a -> f a >>= g
    let (<$>) f m = m >>= (fun a -> M.return (f a))
  end
end

module type I2 = sig
  type ('a, 'e) t
  val return : 'a -> ('a, 'e) t
  val bind : ('a -> ('b, 'e) t) -> ('a, 'e) t -> ('b, 'e) t
end

module type S2 = sig
  include I2
  module Infix : sig
    val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val (>>)  : ('a, 'e) t -> ('b, 'e) t -> ('b, 'e) t
    val (>=>) : ('a -> ('b, 'e) t) -> ('b -> ('c, 'e) t) -> 'a -> ('c, 'e) t
    val (<=<) : ('b -> ('c, 'e) t) -> ('a -> ('b, 'e) t) -> 'a -> ('c, 'e) t
  end
end

module Make2 (M : I2) : S2 with type ('a, 'e) t := ('a, 'e) M.t = struct
  let return = M.return
  let bind   = M.bind

  module Infix = struct
    let (>>=) m f = M.bind f m
    let (>>)  m n = m >>= fun _ -> n
    let (>=>) f g = fun a -> f a >>= g
    let (<=<) g f = fun a -> f a >>= g
    let (<$>) f m = m >>= (fun a -> M.return (f a))
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
end
