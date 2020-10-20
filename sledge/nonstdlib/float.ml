(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Stdlib.Float

external ( = ) : float -> float -> bool = "%equal"
external ( <> ) : float -> float -> bool = "%notequal"
external ( < ) : float -> float -> bool = "%lessthan"
external ( > ) : float -> float -> bool = "%greaterthan"
external ( <= ) : float -> float -> bool = "%lessequal"
external ( >= ) : float -> float -> bool = "%greaterequal"

let of_string_exn = of_string
let of_string = of_string_opt
