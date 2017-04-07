module Fn = struct
  module Category = Category.Make(struct
    type ('a, 'b) t = ('a -> 'b)
    let id = fun x -> x
    let compose f g x = f (g x)
  end)
end

let (%) f g = Fn.Category.compose f g

module Option = struct
  module Functor = Functor.Make(struct
    type 'a t = 'a option

    let map f = function
      | None   -> None
      | Some a -> Some (f a)
  end)

  module Applicative = Applicative.Make(struct
    type 'a t = 'a option

    let pure a = Some a

    let apply f a =
      match f with
      | None   -> None
      | Some f -> Functor.map f a
  end)

  module Monad = Monad.Make(struct
    type 'a t = 'a option

    let return = Applicative.pure

    let bind f = function
      | None   -> None
      | Some a -> f a
  end)
end

module List = struct
  module Functor = Functor.Make(struct
    type 'a t = 'a list

    let map = List.map
  end)

  module Applicative = Applicative.Make(struct
    type 'a t = 'a list

    let pure a = [a]

    let apply fs xs =
      List.map2 (@@) fs xs
  end)

  module Monad = Monad.Make(struct
    type 'a t = 'a list

    let return = Applicative.pure

    let bind f =
      List.concat % (List.map f)
  end)
end

module Result = struct
  module Functor = Functor.Make2(struct
    type ('a, 'b) t = ('a, 'b) result

    let map f = function
      | Ok a         -> Ok (f a)
      | Error _ as e -> e
  end)

  module Bifunctor = Bifunctor.Make(struct
    type ('a, 'b) t = ('a, 'b) result

    let bimap f g = function
      | Ok    a -> Ok    (f a)
      | Error b -> Error (g b)
  end)

  module Applicative = Applicative.Make2(struct
    type ('a, 'b) t = ('a, 'b) result
    let pure a = Ok a
    let apply f a =
      match f with
      | Error _ as e -> e
      | Ok         f ->
          match a with
          | Error _ as e -> e
          | Ok         a -> Ok (f a)

  end)

  module Monad = Monad.Make2(struct
    type ('a, 'b) t = ('a, 'b) result
    let return = Applicative.pure
    let bind f = function
      | Ok         a -> f a
      | Error _ as e -> e
  end)
end

module Pair = struct
  module Functor = Functor.Make2(struct
    type ('a, 'b) t = 'a * 'b
    let map f (a, b) = (f a, b)
  end)

  module Bifunctor = Bifunctor.Make(struct
    type ('a, 'b) t = 'a * 'b
    let bimap f g (a, b) = (f a, g b)
  end)
end
