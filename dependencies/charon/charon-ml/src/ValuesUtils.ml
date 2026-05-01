open Values

let literal_as_scalar (v : literal) : scalar_value =
  match v with
  | VScalar v -> v
  | _ -> raise (Failure "Unexpected")

let literal_type_is_integer (t : literal_type) : bool =
  match t with
  | TInt _ -> true
  | TUInt _ -> true
  | _ -> false

let integer_type_of_literal (v : literal) : integer_type option =
  match v with
  | VScalar (UnsignedScalar (intty, _)) -> Some (Unsigned intty)
  | VScalar (SignedScalar (intty, _)) -> Some (Signed intty)
  | _ -> None
