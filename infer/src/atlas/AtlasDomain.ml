open! IStd

module Address = struct

  type t =
    | NonTop of int
    | Top
  let of_int (n: int) = NonTop n

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | _, Top -> true
    | Top, NonTop _ -> false
    | NonTop a, NonTop b -> a <= b

  let equal x y = leq ~lhs:x ~rhs:y && leq ~lhs:y ~rhs:x

  let add x y =
    match x, y with
    | _, Top | Top, _ -> Top
    | NonTop a, NonTop b -> NonTop (a + b)

  let join a b =
    match (a, b) with
    | Top, _ | _, Top -> Top
    | NonTop x, NonTop y -> NonTop (max x y)

  let widening_threshold = 5

  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Top, _ | _, Top -> Top
    | NonTop prev, NonTop next
      when num_iters < widening_threshold ->
        NonTop (max prev next)
    | NonTop _, NonTop _ -> Top

  let pp fmt = function
    | Top -> Format.pp_print_string fmt "Top"
    | NonTop n -> Format.pp_print_int fmt n
end

module BlockId = struct

    type t = int

    let counter = ref 0

    let fresh () = incr counter; !counter

    let compare : t -> t -> int = Int.compare

    let equal (x : t) (y : t) : bool = Int.equal x y

    let pp = Format.pp_print_int
  end

module Value = struct
  type t =
    | Int of int
    | Ptr of { block : BlockId.t; offset : Address.t }
    | Top

  let of_int n = Int n

  let of_ptr block offset = Ptr { block = block; offset = offset }

  let of_addr addr = 
    match addr with
    | Address.NonTop n -> Int n
    | Address.Top -> Top

  let eval_binop op v1 v2 = 
    match op, v1, v2 with
    | Binop.PlusA _, Int a, Int b -> of_int (a + b)
    | Binop.PlusPI, Ptr p, Int x ->
      Ptr { p with offset = Address.add p.offset (Address.NonTop x) }
    | Binop.PlusPI, Ptr p, Top ->
      Ptr { p with offset = Address.Top }
    | Binop.MinusA _, Int a, Int b -> of_int (a - b)
    | Binop.MinusPI, Ptr p, Int x ->
      Ptr { p with offset = Address.add p.offset (Address.NonTop (-x)) }
    | _, _, _ -> Top

  let leq ~lhs ~rhs =
    match lhs, rhs with
    | _, Top -> true
    | Top, _ -> false
    | Int a, Int b -> a <= b
    | Ptr id1, Ptr id2
      when BlockId.equal id1.block id2.block ->
        Address.leq ~lhs:id1.offset ~rhs:id2.offset
    | _, _ -> false

  let join v1 v2 =
    match v1, v2 with
    | Top, _ | _, Top -> Top
    | Int a, Int b -> Int (max a b)
    | Ptr id1, Ptr id2
      when BlockId.equal id1.block id2.block ->
        Ptr 
          { block = id1.block
          ; offset = Address.join id1.offset id2.offset }
    | Ptr _, Ptr _ -> Top
    | _, _ -> Top

  let widening_threshold = 5

  let widen ~prev ~next ~num_iters =
    match prev, next with
    | Top, _ | _, Top -> Top
    | Int a, Int b
      when num_iters < widening_threshold ->
        Int (max a b)
    | Int _, Int _ -> Top
    | Ptr id1, Ptr id2
      when BlockId.equal id1.block id2.block ->
        Ptr
          { block = id1.block
          ; offset = 
            Address.widen
              ~prev:id1.offset
              ~next:id2.offset
              ~num_iters }
    | _, _ -> Top

  let pp fmt = function
    | Top -> Format.pp_print_string fmt "Top"
    | Int n -> Format.fprintf fmt "Int(%d)" n
    | Ptr { block; offset } ->
      Format.fprintf fmt "Ptr(block=%a, off=%a)"
      BlockId.pp block
      Address.pp offset

end

module Block = struct
  type t = 
    { base : Address.t
    ; end_ : Address.t
    ; freed : bool }
  
  let pp fmt block =
    Format.fprintf fmt "{base=%a; end=%a; freed=%b}"
      Address.pp block.base
      Address.pp block.end_
      block.freed

  let leq ~lhs ~rhs =
    Address.leq ~lhs:lhs.base ~rhs:rhs.base &&
    Address.leq ~lhs:lhs.end_ ~rhs:rhs.end_

  let join block1 block2 =
    { base = Address.join block1.base block2.base
    ; end_ = Address.join block1.end_ block2.end_
    ; freed = block1.freed || block2.freed }
  
  let widen ~prev ~next ~num_iters =
    { base = Address.widen ~prev:prev.base ~next:next.base ~num_iters
    ; end_ = Address.widen ~prev:prev.end_ ~next:next.end_ ~num_iters
    ; freed = prev.freed || next.freed }
end

module Heap = AbstractDomain.Map (BlockId) (Block)

(* Define VarKey since Var does not satisfy IStdlib.PrettyPrintable.PrintableOrderedType *)
module VarKey : PrettyPrintable.PrintableOrderedType = struct
  type t = Var.t

  let compare = Var.compare
  let pp = Var.pp
end

module Env : AbstractDomain.MapS
  with type key = VarKey.t
  and type value = Value.t =
  AbstractDomain.Map (VarKey) (Value)

module Domain = struct
  type t =
    { env: Env.t
    ; heap: Heap.t 
    ; heap_cursor : Address.t}
  
  let init =
    { env = Env.empty
    ; heap = Heap.empty
    ; heap_cursor = Address.of_int 0 }

  let leq ~lhs ~rhs = 
    Env.leq ~lhs:lhs.env ~rhs:rhs.env &&
    Heap.leq ~lhs:lhs.heap ~rhs:rhs.heap

  let join lhs rhs =
    { env = Env.join lhs.env rhs.env
    ; heap = Heap.join lhs.heap rhs.heap
    ; heap_cursor = Address.join lhs.heap_cursor rhs.heap_cursor}

  let widen ~prev ~next ~num_iters =
    { env = Env.widen ~prev:prev.env ~next:next.env ~num_iters
    ; heap= Heap.widen ~prev:prev.heap ~next:next.heap ~num_iters 
    ; heap_cursor = Address.widen ~prev:prev.heap_cursor ~next:next.heap_cursor ~num_iters }

  let pp fmt {env; heap; heap_cursor} =
    Format.fprintf fmt "@[<v2>Env=%a@;Blocks=%a@;Cursor=%a@]" Env.pp env Heap.pp heap Address.pp heap_cursor
end

type t = Domain.t

let empty = Domain.init

let leq ~lhs ~rhs = Domain.leq ~lhs ~rhs

let join = Domain.join

let widen ~prev ~next ~num_iters = Domain.widen ~prev ~next ~num_iters

let pp fmt astate = Domain.pp fmt astate

let alloc_block (size: Value.t) (astate: t) : t * Value.t =
  let id = BlockId.fresh () in
  let size' =
    match size with
    | Value.Int s -> Address.NonTop s
    | Value.Ptr _ | Value.Top -> Address.Top
  in
  let base = astate.heap_cursor in
  let new_cursor =
    match base, size' with
    | Address.NonTop b, Address.NonTop s ->
      Address.of_int (b + s + 1)
    | Address.Top, _ | _, Address.Top ->
      Address.Top
  in
  let block =
    let open Block in
    { base = base
    ; end_ =
      (match base, size' with
      | Address.NonTop b, Address.NonTop s ->
        Address.of_int (b + s)
      | Address.Top, _ | _, Address.Top ->
        Address.Top )
    ; freed = false }
  in
  let new_blocks = Heap.add id block astate.heap in
  { astate with heap = new_blocks; heap_cursor = new_cursor },
  Value.Ptr { block = id; offset = Address.NonTop 0 }

let free_block (id: BlockId.t) (astate: t) : t * bool =
  let block = Heap.find_opt id astate.heap in
  let double_free =
    match block with
    | Some { base = _; end_ = _; freed = true } -> true
    | Some _ | None -> false
  in 
  let heap' =
    (Heap.update id
      (function 
        | Some block ->
          Some { block with freed = true }
        | None -> None)
      astate.heap)
  in
  { astate with heap = heap' }, double_free

let is_freed (id: BlockId.t) (astate: t) : bool =
  let block = Heap.find_opt id astate.heap in
  match block with
  | Some { base = _; end_ = _; freed = true } ->
    true
  | _ ->
    false 

let base (loc: Address.t) (astate: t) : Address.t option =
  Heap.fold
    (fun _ block acc ->
      match acc with
      | Some _ -> acc
      | None ->
        match (block.base, block.end_, loc) with
        | (Address.NonTop b, Address.NonTop e, Address.NonTop l)
          when b <= l && l <= e ->
            Some block.base
        | _ ->
          None)
    astate.heap
    None

let end_ (loc:Address.t) (astate: t) : Address.t option =
  Heap.fold
    (fun _ block acc ->
      match acc with
      | Some _ -> acc
      | None ->
        match (block.base, block.end_, loc) with
        | (Address.NonTop b, Address.NonTop e, Address.NonTop l)
          when b <= l && l <= e ->
            Some block.end_
        | _ ->
          None)
    astate.heap
    None

let key_of_var (var : Var.t) : Env.key =
  Obj.magic var

let lookup_var (var : Var.t) (astate : t) : Value.t =
  let key = key_of_var var in
  match Env.find_opt key astate.env with
    | Some v -> v
    | None -> Value.Top

let store_var (var : Var.t) (value: Value.t) (astate: t) : t =
  let key = key_of_var var in
  { astate with env= Env.add key value astate.env }
