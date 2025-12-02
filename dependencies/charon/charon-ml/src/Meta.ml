(** Meta data like code spans *)
include Generated_Meta

(** Ordered file name *)
module OrderedFileName : Collections.OrderedType with type t = file_name =
struct
  type t = file_name

  let compare = compare_file_name
  let to_string s = show_file_name s
  let pp_t fmt s = pp_file_name fmt s
  let show_t s = show_file_name s
end

module FileNameSet = Collections.MakeSet (OrderedFileName)
module FileNameMap = Collections.MakeMap (OrderedFileName)
