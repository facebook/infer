(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include (
  Core :
    sig
      include
        module type of Core
          with module Printexc := Core.Printexc
           and module Option := Core.Option
           and module List := Core.List
           and module Map := Core.Map
           and module Set := Core.Set
           and module String := Core.String
           and type -'a return := 'a Core.return
    end )

external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

(** Function combinators *)

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let ( $ ) f g x = f x ; g x
let ( $> ) x f = f x ; x
let ( <$ ) f x = f x ; x

(** Pretty-printer for argument type. *)
type 'a pp = Format.formatter -> 'a -> unit

(** Format strings. *)
type ('a, 'b) fmt = ('a, 'b) Trace.fmt

module type Applicative_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module type Monad_syntax = sig
  include Applicative_syntax

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end
