val list : ('a -> 'a -> bool) -> 'a list -> 'a list[@@ocaml.deprecated
                                                     "Use List.sort instead."]
val array : ('a -> 'a -> bool) -> 'a array -> unit[@@ocaml.deprecated
                                                    "Use Array.sort instead."]
val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list[@@ocaml.deprecated
                                                                 "Use List.merge instead."]
