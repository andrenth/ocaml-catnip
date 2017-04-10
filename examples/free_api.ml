open Catnip
open Catnip.Prelude

(** We consider an API with three types of effectful components: a persistent
    layer, a message broker, and a logger module. This module develops an
    interface using the Interpreter Pattern and Free Monads.

	  For each of these components, we define a DSL that consists of a sum type
    and a [map] function (therefore, a functor in the category theory sense).
	  This functor is used to obtain a Free Monad.

	  This approach allows us to model effectful computations using pure
    functions, and use the type system to determine what kinds of effects a
    given function can perform (barring, of course, usage of impure functions
    in the APIs, which in OCaml must be forbidden by convention).

    The example also shows how to combine multiple DSLs using Coproducts, for
    situations where APIs need to perform different kinds of effects.

    Finally, example interpreters that simply print the actions they would
    perform are given. *)


(** The persistence layer. In the real world, it might make sense to define
    separate DSLs for Commands and Queries if it's important to be able to
    determine, using the type system, which functions modify the database vs.
    ones that simply read from it, say, when using CQRS. *)
module Persist = struct
  module DSL = struct
    type 'a t =
      | Command of string * (unit -> 'a)
      | Query   of string * (string option -> 'a)
      (** A sum type modeling database-related actions. The [Command]
          case is used for database modifications, while the [Query] case
          is for fetching data. The [string]s represent queries in the
          appropriate database language, and should probably be replaced
          by more appropriate types. The functions are the continuations
          that allow us to build a recursive data structure out of the
          DSL, to be interpreted later. Notice that the [Command] case has
          no meaningful return value here, so the continuation takes a [unit]
          parameter, while the [Query] case may return a value, here
          represented as a [string option]. *)

    let map f = function
      | Command (cmd, k) -> Command (cmd, f % k)
      | Query   (qry, k) -> Query   (qry, f % k)
      (** We need our DSL to be a functor (in the category theory sense) so
          we define [map] for it. *)

    let command c = Command (c, id)
      (** Smart constructor for commands. *)
    let query   q = Query   (q, id)
      (** Smart constructor for queries. *)
  end

  (** This (OCaml) functor creates a natural transformation (see module type
      [Functor.T]) from our DSL into some other (categorical) functor. *)
  module Lift (T : Functor.T with module From := DSL) = struct
    let command = T.trans % DSL.command
    let query   = T.trans % DSL.query
  end

  (** [Free] turns our DSL into a monad. *)
  module Free = Monad.Free.Make (DSL)

  (** Transform DSL types into Free types, allowing its use in monadic
      context. *)
  include Lift (struct
    module Into = Free
    let trans = Free.lift
  end)
end

(** The message broker interface. The DSL defined here is pretty much the
    same as the one in [Persist], using, of course, the appropriate domain
    language. *)
module Broker = struct
  module DSL = struct
    type 'a t =
      | Send of string * (unit -> 'a)
      | Recv of (string option -> 'a)

    let map f = function
      | Send (msg, k) -> Send (msg, f % k)
      | Recv k        -> Recv (f % k)

    let send msg = Send (msg, id)
    let recv k   = Recv k
  end

  module Lift (T : Functor.T with module From := DSL) = struct
    let send   = T.trans % DSL.send
    let recv k = (T.trans % DSL.recv) k
  end

  module Free = Monad.Free.Make (DSL)

  include Lift (struct
    module Into = Free
    let trans = Free.lift
  end)
end

(** Finally, the logger. We define a really simple one, since the purpose
    here is simply to show how to combine three or more APIs, as will be
    shown later. *)
module Log = struct
  module DSL = struct
    type 'a t =
      | Info of string * (unit -> 'a)

    let map f = function
      | Info (msg, k) -> Info (msg, f % k)

    let info msg = Info (msg, id)
  end

  module Lift (T : Functor.T with module From := DSL) = struct
    let info = T.trans % DSL.info
  end

  module Free = Monad.Free.Make (DSL)

  include Lift (struct
    module Into = Free
    let trans = Free.lift
  end)
end

(** Here we define an API that can call bother [Persist] and [Broker]
    functions. The combined DSL is obtained via the coproduct (a.k.a. sum)
    of the two base DSLs. *)
module Persist_with_Broker = struct
  (** We get the combined DSL with a coproduct. This works because the sum of
      two (categorical) functors is also a functor. *)
  module DSL  = Functor.Coproduct.Make (Persist.DSL) (Broker.DSL)
  (** The Free Monad for the combined DSL. *)
  module Free = Monad.Free.Make (DSL)

	(** We must lift a [Persist.DSL] into [Free]. First we lift [Persist.DSL]
      into [DSL] by using [lift_left] (as [Persist.DSL] is the left argument
      given in the application of [Coproduct.Make], above, then we lift the
      result into [Free]. *)
  module Persist = struct
    include Persist.Lift (struct
      module Into = Free
      let trans dsl =
        dsl |> DSL.lift_left
            |> Free.lift
    end)
  end

  (** Analogously, we lift the [Broker.DSL] into [Free] by first lifting it
      to [DSL], this time going right. *)
  module Broker = struct
    include Broker.Lift (struct
      module Into = Free
      let trans dsl =
        dsl |> DSL.lift_right
            |> Free.lift
    end)
  end
end

(** We now add [Log] to the mix, allowing functions from the three DSLs to
    be called from the same function. Another coproduct is created from
    [Persist_with_Broker.DSL], defined above and [Log.DSL]. *)
module Persist_with_Broker_and_Log = struct
  (** The DSL combining our three base DSLs. *)
  module DSL  = Functor.Coproduct.Make (Persist_with_Broker.DSL) (Log.DSL)
  (** The Free Monad for our DSL. *)
  module Free = Monad.Free.Make (DSL)

  (** Once again, [Persist.DSL] must be lifted into [Free]. First we go left
      to lift it into [Persist_with_Broker.DSL], then left again to lift it
      into [DSL] and finally lift it into [Free]. *)
  module Persist = struct
    include Persist.Lift (struct
      module Into = Free
      let trans dsl =
        dsl |> Persist_with_Broker.DSL.lift_left
            |> DSL.lift_left
            |> Free.lift
    end)
  end

  (** Similarly, [Broker.DSL] is lifted into [Free] by first going right on
      [Persist_with_Broker.DSL], then left on [DSL] and finally lifting it
      into [Free]. *)
  module Broker = struct
    include Broker.Lift (struct
      module Into = Free
      let trans dsl =
        dsl |> Persist_with_Broker.DSL.lift_right
            |> DSL.lift_left
            |> Free.lift
    end)
  end

  (** Finally, we lift [Log.DSL]. *)
  module Log = struct
    include Log.Lift (struct
      module Into = Free
      let trans dsl =
        dsl |> DSL.lift_right
            |> Free.lift
    end)
  end
end


(** Example functions. *)

let persist_only =
  let open Persist in
  let open Free.Infix in
  Persist.query "select * from t limit 1" >>
  Persist.command "delete from t"

let persist_with_broker =
  let open Persist_with_Broker in
  let open Free in
  let open Infix in
  Persist.query "select * from t limit 1" >>=
  (function
  | Some s -> Broker.send ("{ \"blah\": " ^ s ^ " }")
  | None -> return ()) >>
  Persist.command "delete from t"

let persist_with_broker_and_log =
  let open Persist_with_Broker_and_Log in
  let open Free in
  let open Infix in
  Log.info "selecting" >>
  Persist.query "select * from t limit 1"
  >>=
  (function
  | Some s ->
      Log.info "sending message" >>
      Broker.send ("{ \"blah\": " ^ s ^ " }")
  | None -> return ())
  >>
  Log.info "deleting" >>
  Persist.command "delete from t"

(** Interpreters must define a single function, by convention called [run],
    with type ['a Free.t -> 'a]. *)
module Interpreters = struct
  module Debug_Persist = struct
    open Persist

    let rec run = function
      | Free.Pure a -> a
      | Free.Free u ->
          match u with
          | DSL.Command (c, k) ->
              print_endline @@ "command: " ^ c;
              run (k ())
          | DSL.Query (q, k) ->
              print_endline @@ "query: " ^ q;
              run (k (Some "example"))
  end

  module Debug_Broker = struct
    open Broker

    let rec run = function
      | Free.Pure a -> a
      | Free.Free u ->
          match u with
          | DSL.Send (msg, k) ->
              print_endline @@ "sending message: " ^ msg;
              run (k ())
          | DSL.Recv k ->
              print_endline @@ "receiving message";
              run (k (Some "{ \"dummy\": true }"))
  end

  module Debug_Log = struct
		open Log

    let rec run = function
      | Free.Pure a -> a
      | Free.Free u ->
          match u with
          | DSL.Info (msg, k) ->
              print_endline @@ "info: " ^ msg;
              run (k ())
  end

	(** Interpreters for combined DSL can be built from base interpreters. *)

  module Debug_Persist_with_Broker = struct
		module DSL = Persist_with_Broker.DSL

    let run_free = function
      | DSL.Left  persist -> Debug_Persist.run (Persist.Free.lift persist)
      | DSL.Right broker  -> Debug_Broker.run (Broker.Free.lift broker)

    let run free =
      Persist_with_Broker.Free.iter run_free free
  end

  module Debug_Persist_with_Broker_and_Log = struct
		module DSL = Persist_with_Broker_and_Log.DSL

    let run_free = function
      | DSL.Left p_with_b ->
					Debug_Persist_with_Broker.run (Persist_with_Broker.Free.lift p_with_b)
      | DSL.Right log ->
					Debug_Log.run (Log.Free.lift log)

    let run free =
      Persist_with_Broker_and_Log.Free.iter run_free free
  end
end

open Interpreters

let () =
  print_endline "=== Persist-only";
  Debug_Persist.run persist_only

let () =
  print_endline "=== Persist with Broker";
  Debug_Persist_with_Broker.run persist_with_broker


let () =
  print_endline "=== Persist with Broker and Log";
  Debug_Persist_with_Broker_and_Log.run persist_with_broker_and_log
