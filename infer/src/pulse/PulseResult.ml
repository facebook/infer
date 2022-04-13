(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type ('ok, 'err) t = Ok of 'ok | Recoverable of 'ok * 'err list | FatalError of 'err * 'err list

let append_errors errors result =
  if List.is_empty errors then result
  else
    match result with
    | Ok x ->
        Recoverable (x, errors)
    | Recoverable (x, errors') ->
        Recoverable (x, errors' @ errors)
    | FatalError (fatal, errors') ->
        FatalError (fatal, errors' @ errors)


let ok = function Ok x -> Some x | FatalError _ | Recoverable _ -> None

let ok_exn = function
  | Ok x ->
      x
  | Recoverable _ | FatalError _ ->
      L.die InternalError "ok_exn of something not Ok"


let map x ~f =
  match x with
  | Ok x' ->
      Ok (f x')
  | Recoverable (x', errors) ->
      Recoverable (f x', errors)
  | FatalError _ as err ->
      err


let map_error x ~f =
  match x with
  | Ok _ as x ->
      x
  | Recoverable (x', errors) ->
      Recoverable (x', List.map ~f errors)
  | FatalError (fatal, errors) ->
      FatalError (f fatal, List.map ~f errors)


let bind x ~f =
  match x with
  | Ok x' ->
      f x'
  | FatalError _ as err ->
      err
  | Recoverable (x', errors) -> (
    match f x' with
    | Ok x'' ->
        Recoverable (x'', errors)
    | Recoverable (x'', errors') ->
        Recoverable (x'', errors' @ errors)
    | FatalError (fatal, errors') ->
        FatalError (fatal, errors' @ errors) )


let join result_result = bind result_result ~f:Fn.id

module Type = struct
  type ('ok, 'err) pulse_result = ('ok, 'err) t =
    | Ok of 'ok
    | Recoverable of 'ok * 'err list
    | FatalError of 'err * 'err list
end

module Monad_infix = struct
  include Type

  let ( >>| ) x f = map x ~f

  let ( >>= ) x f = bind x ~f
end

module Let_syntax = struct
  include Monad_infix

  let ( let+ ) x f = map x ~f

  let ( let* ) x f = bind x ~f
end

open Let_syntax

let of_some = function
  | Ok None | Recoverable (None, _) ->
      None
  | Ok (Some x) ->
      Some (Ok x)
  | Recoverable (Some x, errors) ->
      Some (Recoverable (x, errors))
  | FatalError _ as err ->
      Some err


let recoverable_of_result ~ok_of_error = function
  | (Ok x : _ result) ->
      Ok x
  | Error err ->
      Recoverable (ok_of_error err, [err])


let fatal_of_result = function (Ok x : _ result) -> Ok x | Error err -> FatalError (err, [])

let to_result : _ t -> _ result = function
  | Ok astate ->
      Ok astate
  | Recoverable (_, errors) ->
      Error errors
  | FatalError (error, errors) ->
      Error (error :: errors)


let list_fold l ~init ~f =
  List.fold l ~init:(Ok init) ~f:(fun result x ->
      let* acc = result in
      f acc x )


let list_fold2 l1 l2 ~init ~f =
  List.fold2 l1 l2 ~init:(Ok init) ~f:(fun result x1 x2 ->
      let* acc = result in
      f acc x1 x2 )


let list_foldi ~init ~f l =
  List.foldi l ~init:(Ok init) ~f:(fun i result x ->
      let* acc = result in
      f i acc x )


let container_fold :
       fold:('t, 'a, ('accum, 'err) t) Container.fold
    -> 't
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'err) t)
    -> ('accum, 'err) t =
 fun ~fold container ~init ~f ->
  fold container ~init:(Ok init) ~f:(fun result x ->
      let* acc = result in
      f acc x )
