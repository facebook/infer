val enabled : bool
module Series :
sig
  type t
  val create : path:string -> t
  val save_event : ?time:float -> t -> event_name:string -> unit
  val save_and_close : ?time:float -> t -> unit
end
module Snapshot : sig val take : ?time:float -> Series.t -> unit end
val save_event_for_automatic_snapshots : event_name:string -> unit
