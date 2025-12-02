exception IntegerOverflow of unit
exception Unimplemented

(** Utility exception

    When looking for something while exploring a term, it can be easier to just
    throw an exception to signal we found what we were looking for. *)
exception Found

(** Union Find *)
module UF = UnionFind.Make (UnionFind.StoreMap)
