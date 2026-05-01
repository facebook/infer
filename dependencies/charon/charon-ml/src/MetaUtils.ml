open Meta

let loc_min (l0 : loc) (l1 : loc) : loc =
  if l0.line = l1.line then { line = l0.line; col = Int.min l0.col l1.col }
  else if l0.line < l1.line then l0
  else l1

let loc_max (l0 : loc) (l1 : loc) : loc =
  if l0.line = l1.line then { line = l0.line; col = Int.max l0.col l1.col }
  else if l0.line > l1.line then l0
  else l1

(** See the comments in [meta_utils.rs] in Charon. *)
let combine_span (m0 : span) (m1 : span) : span option =
  if m0.data.file = m1.data.file then
    let data =
      {
        file = m0.data.file;
        beg_loc = loc_min m0.data.beg_loc m1.data.beg_loc;
        end_loc = loc_max m0.data.end_loc m1.data.end_loc;
      }
    in
    Some { data; generated_from_span = None }
  else None

(** Safely combine two spans, even if they do not come from the same file - if
    this happens, we simply use the first span. *)
let safe_combine_span (m0 : span) (m1 : span) : span =
  match combine_span m0 m1 with
  | None -> m0
  | Some m -> m

module OrderedSpan : Collections.OrderedType with type t = span = struct
  type t = span

  let compare = compare_span
  let to_string = show_span
  let pp_t fmt x = Format.pp_print_string fmt (show_span x)
  let show_t = show_span
end

module SpanSet = Collections.MakeSet (OrderedSpan)
module SpanMap = Collections.MakeMap (OrderedSpan)
