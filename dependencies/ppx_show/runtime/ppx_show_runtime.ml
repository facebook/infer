module Format = Format

module String = String

module Int32 = Int32

module Int64 = Int64

module Nativeint = Nativeint

module Bytes = Bytes

module Lazy = Lazy

let pp_list pp_item fmt items =
  Format.pp_open_box fmt 1;
  Format.pp_print_string fmt "[";
  begin match items with
  | [] -> ()
  | hd :: tl ->
      pp_item fmt hd;
      tl |> List.iter begin fun item ->
        Format.pp_print_string fmt ";";
        Format.pp_print_space fmt ();
        pp_item fmt item
      end
  end;
  Format.pp_print_string fmt "]";
  Format.pp_close_box fmt ()
