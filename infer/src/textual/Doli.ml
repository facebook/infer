(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module Java = struct
  type modifier = Public | Private | Protected | Static | Final | Abstract | Native
  [@@deriving equal]

  type basicType =
    | IntType
    | BoolType
    | ByteType
    | CharType
    | DoubleType
    | FloatType
    | LongType
    | ShortType

  type typeArgument = PLAIN of referenceType | SUPER of referenceType | EXTENDS of referenceType

  and referenceType = RT of classType list

  and classType = CT of (string * typeArgument list)

  and nonVoidType = RefType of referenceType | BasicType of basicType | Array of nonVoidType

  and returnType = VoidType | NonVoid of nonVoidType

  type formalParameter = {typ: nonVoidType; ident: string}

  type signature =
    { modifiers: modifier list
    ; returns: returnType
    ; identifier: string
    ; formalParameters: formalParameter list }

  type extendedSignature = {signs: signature list; under: referenceType}

  let is_static (sign : signature) : bool = List.mem ~equal:equal_modifier sign.modifiers Static

  let get_func_identifier_simple (sign : signature) : string = sign.identifier

  let reference_type_to_textual (RT clts) : Textual.Typ.t =
    (* FIXME if the reference type is a path, then the function will need to
       trace and translate all of that path. Instead, we are currentlt ignoring such
       paths. *)
    let rec last_class_type_to_textual (cts : classType list) : Textual.Typ.t =
      match cts with
      | [] ->
          L.die InternalError "Class Types should never be empty "
      | [CT (strng, _)] ->
          Struct {value= strng; loc= Unknown} (*FIXME--  get the location  *)
      | _ :: cts ->
          last_class_type_to_textual cts
    in
    last_class_type_to_textual clts


  let rec non_void_type_to_textual (nvt : nonVoidType) : Textual.Typ.t =
    let basic_type_to_textual (bt : basicType) : Textual.Typ.t =
      match bt with
      | IntType ->
          Textual.Typ.Int
      | FloatType ->
          Textual.Typ.Float
      | _ ->
          L.die ExternalError "we have no representation for this Java basic type, yet."
      (* QUESTION what do we do with the remaining Java types?
         perhaps not relevant, as we want to inser doli-defined function into HLL code --
         and not into textual calls! *)
    in
    match nvt with
    | RefType rt ->
        Ptr (reference_type_to_textual rt)
    | BasicType bt ->
        basic_type_to_textual bt
    | Array nvt ->
        Ptr (Array (non_void_type_to_textual nvt))


  let return_type_to_textual (extSigns : extendedSignature list) : Textual.Typ.t =
    (* FIXME we should calculate the common supertype of the return types of all the signatures
       in all the extended signatures from the list, and raise an exception if none exists.
       Instaed, currently, we are only looking at the first element of the first element.
       OR, we should require that all signatures have the same return type. *)
    let return_type_to_textual (rt : returnType) : Textual.Typ.t =
      match rt with VoidType -> Void | NonVoid nvt -> non_void_type_to_textual nvt
    in
    match extSigns with
    | extSign :: _ -> (
      match extSign.signs with
      | [] ->
          L.die InternalError "Sigatures in a doli-extended signature cannot be empty"
      | sign :: _ ->
          return_type_to_textual sign.returns )
    | [] ->
        L.die InternalError "Doli-extended signature cannot be empty"


  let param_types_to_textual (extSigns : extendedSignature list) : Textual.Typ.annotated list
      (* FIXME we currently take the first signature from the first externded signatue; but
          we should instead be taking the common suypertypes for all these
          Or we should require that all signatures have the same parameter types *) =
    let make_annotated_type (fp : formalParameter) : Textual.Typ.annotated =
      (* FIXME revisit the values of the attribute list *)
      {typ= non_void_type_to_textual fp.typ; attributes= []}
    in
    match extSigns with
    | extSign :: _ -> (
      match extSign.signs with
      | [] ->
          L.die InternalError "Sigatures in a doli-extended signature cannot be empty"
      | sign :: _ ->
          let formal_argument_types = List.map ~f:make_annotated_type sign.formalParameters in
          if is_static sign then formal_argument_types
          else
            let receiverType = Textual.Typ.Ptr (reference_type_to_textual extSign.under) in
            {typ= receiverType; attributes= []} :: formal_argument_types )
    | [] ->
        L.die InternalError "Doli-extended signatures cannot be empty"


  let get_parameter_names (extd_signs : extendedSignature list) : Textual.VarName.t list =
    (* FIXME we should probably require that all signatures have the same parameter names,
       otherwise we would need to check each signature separately, and create a different
       ProcDesc for each one. *)
    let make_parameter_name (fp : formalParameter) : Textual.VarName.t =
      (* FIXME revisit the valueof the loc *)
      {value= fp.ident; loc= Unknown}
    in
    match extd_signs with
    | extSign :: _ -> (
      match extSign.signs with
      | [] ->
          L.die InternalError "Sigatures in a doli-extended signature cannot be empty"
      | sign :: _ ->
          let formal_argument_names = List.map ~f:make_parameter_name sign.formalParameters in
          if is_static sign then formal_argument_names
          else {value= "this"; loc= Unknown} :: formal_argument_names )
    | [] ->
        L.die InternalError "Doli-extended signatures cannot be empty"
end

module ObjC = struct
  type extendedSignature = {stub: int}
end

type matching =
  | JavaMatching of Java.extendedSignature list
  | ObjCMatching of ObjC.extendedSignature list

type doliRule = {ruleName: string; match_: matching; body: Textual.Body.t}

type doliProgram = DoliProgram of doliRule list

let return_type_to_textual (doliRule : doliRule) : Textual.Typ.t =
  match doliRule.match_ with
  | JavaMatching extSigns ->
      Java.return_type_to_textual extSigns
  | _ ->
      L.die InternalError "ObjectiveC doli rules not being handled yet"


let param_types_to_textual (doliRule : doliRule) : Textual.Typ.annotated list =
  match doliRule.match_ with
  | JavaMatching extSigns ->
      Java.param_types_to_textual extSigns
  | _ ->
      L.die InternalError "ObjC doli rules not being handled yet"


let get_parameter_names (doliRule : doliRule) : Textual.VarName.t list =
  match doliRule.match_ with
  | JavaMatching extSigns ->
      Java.get_parameter_names extSigns
  | _ ->
      L.die InternalError "ObjC doli rules not being handled yet"
