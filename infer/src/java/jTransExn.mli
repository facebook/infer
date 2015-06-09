open Javalib_pack
open Sawja_pack


val create_exception_handlers : JContext.t -> Cfg.Node.t list -> (int -> Cfg.Node.t list) -> JBir.t -> int -> Cfg.Node.t list
