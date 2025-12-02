open Expressions

let unop_can_fail : unop -> bool = function
  | Neg (OPanic | OUB) | Cast _ | PtrMetadata -> true
  | Neg OWrap | Not -> false

let binop_can_fail : binop -> bool = function
  | BitXor | BitAnd | BitOr | Eq | Lt | Le | Ne | Ge | Gt
  | Add OWrap
  | Sub OWrap
  | Mul OWrap
  | Shl OWrap
  | Shr OWrap
  | AddChecked | SubChecked | MulChecked | Cmp -> false
  | Div _ | Rem _ | Add _ | Sub _ | Mul _ | Shl _ | Shr _ | Offset -> true
