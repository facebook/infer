open OfJsonBasic
open Types
open Expressions
open UllbcAst
open GAstOfJson

let rec ___ = ()

and block_of_json (ctx : of_json_ctx) (js : json) : (block, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("statements", statements); ("terminator", terminator) ] ->
        let* statements = list_of_json statement_of_json ctx statements in
        let* terminator = terminator_of_json ctx terminator in
        Ok ({ statements; terminator } : block)
    | _ -> Error "")

and block_id_of_json (ctx : of_json_ctx) (js : json) : (block_id, string) result
    =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> BlockId.id_of_json ctx x
    | _ -> Error "")

and statement_of_json (ctx : of_json_ctx) (js : json) :
    (statement, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [ ("span", span); ("kind", kind); ("comments_before", comments_before) ]
      ->
        let* span = span_of_json ctx span in
        let* kind = statement_kind_of_json ctx kind in
        let* comments_before =
          list_of_json string_of_json ctx comments_before
        in
        Ok ({ span; kind; comments_before } : statement)
    | _ -> Error "")

and statement_kind_of_json (ctx : of_json_ctx) (js : json) :
    (statement_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Assign", `List [ x_0; x_1 ]) ] ->
        let* x_0 = place_of_json ctx x_0 in
        let* x_1 = rvalue_of_json ctx x_1 in
        Ok (Assign (x_0, x_1))
    | `Assoc [ ("SetDiscriminant", `List [ x_0; x_1 ]) ] ->
        let* x_0 = place_of_json ctx x_0 in
        let* x_1 = variant_id_of_json ctx x_1 in
        Ok (SetDiscriminant (x_0, x_1))
    | `Assoc [ ("CopyNonOverlapping", copy_non_overlapping) ] ->
        let* copy_non_overlapping =
          box_of_json copy_non_overlapping_of_json ctx copy_non_overlapping
        in
        Ok (CopyNonOverlapping copy_non_overlapping)
    | `Assoc [ ("StorageLive", storage_live) ] ->
        let* storage_live = local_id_of_json ctx storage_live in
        Ok (StorageLive storage_live)
    | `Assoc [ ("StorageDead", storage_dead) ] ->
        let* storage_dead = local_id_of_json ctx storage_dead in
        Ok (StorageDead storage_dead)
    | `Assoc [ ("PlaceMention", place_mention) ] ->
        let* place_mention = place_of_json ctx place_mention in
        Ok (PlaceMention place_mention)
    | `Assoc
        [
          ("Assert", `Assoc [ ("assert", assert_); ("on_failure", on_failure) ]);
        ] ->
        let* assert_ = assertion_of_json ctx assert_ in
        let* on_failure = abort_kind_of_json ctx on_failure in
        Ok (Assert (assert_, on_failure))
    | `String "Nop" -> Ok Nop
    | _ -> Error "")

and switch_of_json (ctx : of_json_ctx) (js : json) : (switch, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("If", `List [ x_0; x_1 ]) ] ->
        let* x_0 = block_id_of_json ctx x_0 in
        let* x_1 = block_id_of_json ctx x_1 in
        Ok (If (x_0, x_1))
    | `Assoc [ ("SwitchInt", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = literal_type_of_json ctx x_0 in
        let* x_1 =
          list_of_json (pair_of_json literal_of_json block_id_of_json) ctx x_1
        in
        let* x_2 = block_id_of_json ctx x_2 in
        Ok (SwitchInt (x_0, x_1, x_2))
    | _ -> Error "")

and terminator_of_json (ctx : of_json_ctx) (js : json) :
    (terminator, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [ ("span", span); ("kind", kind); ("comments_before", comments_before) ]
      ->
        let* span = span_of_json ctx span in
        let* kind = terminator_kind_of_json ctx kind in
        let* comments_before =
          list_of_json string_of_json ctx comments_before
        in
        Ok ({ span; kind; comments_before } : terminator)
    | _ -> Error "")

and terminator_kind_of_json (ctx : of_json_ctx) (js : json) :
    (terminator_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Goto", `Assoc [ ("target", target) ]) ] ->
        let* target = block_id_of_json ctx target in
        Ok (Goto target)
    | `Assoc [ ("Switch", `Assoc [ ("discr", discr); ("targets", targets) ]) ]
      ->
        let* discr = operand_of_json ctx discr in
        let* targets = switch_of_json ctx targets in
        Ok (Switch (discr, targets))
    | `Assoc
        [
          ( "Call",
            `Assoc
              [ ("call", call); ("target", target); ("on_unwind", on_unwind) ]
          );
        ] ->
        let* call = call_of_json ctx call in
        let* target = block_id_of_json ctx target in
        let* on_unwind = block_id_of_json ctx on_unwind in
        Ok (Call (call, target, on_unwind))
    | `Assoc
        [
          ( "Drop",
            `Assoc
              [
                ("kind", kind);
                ("place", place);
                ("tref", tref);
                ("target", target);
                ("on_unwind", on_unwind);
              ] );
        ] ->
        let* kind = drop_kind_of_json ctx kind in
        let* place = place_of_json ctx place in
        let* tref = trait_ref_of_json ctx tref in
        let* target = block_id_of_json ctx target in
        let* on_unwind = block_id_of_json ctx on_unwind in
        Ok (Drop (kind, place, tref, target, on_unwind))
    | `Assoc
        [
          ( "Assert",
            `Assoc
              [
                ("assert", assert_); ("target", target); ("on_unwind", on_unwind);
              ] );
        ] ->
        let* assert_ = assertion_of_json ctx assert_ in
        let* target = block_id_of_json ctx target in
        let* on_unwind = block_id_of_json ctx on_unwind in
        Ok (TAssert (assert_, target, on_unwind))
    | `Assoc [ ("Abort", abort) ] ->
        let* abort = abort_kind_of_json ctx abort in
        Ok (Abort abort)
    | `String "Return" -> Ok Return
    | `String "UnwindResume" -> Ok UnwindResume
    | _ -> Error "")
