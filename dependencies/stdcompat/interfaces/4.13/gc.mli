type stat =
  {
  minor_words: float ;
  promoted_words: float ;
  major_words: float ;
  minor_collections: int ;
  major_collections: int ;
  heap_words: int ;
  heap_chunks: int ;
  live_words: int ;
  live_blocks: int ;
  free_words: int ;
  free_blocks: int ;
  largest_free: int ;
  fragments: int ;
  compactions: int ;
  top_heap_words: int ;
  stack_size: int ;
  forced_major_collections: int }
type control =
  {
  mutable minor_heap_size: int ;
  mutable major_heap_increment: int ;
  mutable space_overhead: int ;
  mutable verbose: int ;
  mutable max_overhead: int ;
  mutable stack_limit: int ;
  mutable allocation_policy: int ;
  window_size: int ;
  custom_major_ratio: int ;
  custom_minor_ratio: int ;
  custom_minor_max_size: int }
external stat : unit -> stat = "caml_gc_stat"
external quick_stat : unit -> stat = "caml_gc_quick_stat"
external counters : unit -> (float * float * float) = "caml_gc_counters"
external minor_words :
  unit -> ((float)[@unboxed ]) = "caml_gc_minor_words"
    "caml_gc_minor_words_unboxed"
external get : unit -> control = "caml_gc_get"
external set : control -> unit = "caml_gc_set"
external minor : unit -> unit = "caml_gc_minor"
external major_slice : int -> int = "caml_gc_major_slice"
external major : unit -> unit = "caml_gc_major"
external full_major : unit -> unit = "caml_gc_full_major"
external compact : unit -> unit = "caml_gc_compaction"
val print_stat : out_channel -> unit
val allocated_bytes : unit -> float
external get_minor_free : unit -> int = "caml_get_minor_free"
external get_bucket : int -> int = "caml_get_major_bucket"[@@noalloc ]
external get_credit : unit -> int = "caml_get_major_credit"[@@noalloc ]
external huge_fallback_count : unit -> int = "caml_gc_huge_fallback_count"
val finalise : ('a -> unit) -> 'a -> unit
val finalise_last : (unit -> unit) -> 'a -> unit
val finalise_release : unit -> unit
type alarm
val create_alarm : (unit -> unit) -> alarm
val delete_alarm : alarm -> unit
external eventlog_pause : unit -> unit = "caml_eventlog_pause"
external eventlog_resume : unit -> unit = "caml_eventlog_resume"
module Memprof :
sig
  type allocation_source =
    | Normal 
    | Marshal 
    | Custom 
  type allocation = private
    {
    n_samples: int ;
    size: int ;
    source: allocation_source ;
    callstack: Printexc.raw_backtrace }
  type ('minor, 'major) tracker =
    {
    alloc_minor: allocation -> 'minor option ;
    alloc_major: allocation -> 'major option ;
    promote: 'minor -> 'major option ;
    dealloc_minor: 'minor -> unit ;
    dealloc_major: 'major -> unit }
  val null_tracker : ('minor, 'major) tracker
  val start :
    sampling_rate:float ->
      ?callstack_size:int -> ('minor, 'major) tracker -> unit
  val stop : unit -> unit
end
