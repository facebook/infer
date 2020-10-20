(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Stdlib.Float

external ( = ) : float -> float -> bool = "%equal"
external ( <> ) : float -> float -> bool = "%notequal"
external ( < ) : float -> float -> bool = "%lessthan"
external ( > ) : float -> float -> bool = "%greaterthan"
external ( <= ) : float -> float -> bool = "%lessequal"
external ( >= ) : float -> float -> bool = "%greaterequal"
val of_string : string -> float option
val of_string_exn : string -> float
