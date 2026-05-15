open OfJsonBasic
open Types
open LlbcAst
open GAstOfJson

let rec ___ = ()

and block_of_json (ctx : of_json_ctx) (js : json) : (block, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("span", span); ("statements", statements) ] ->
        let* span = span_of_json ctx span in
        let* statements = list_of_json statement_of_json ctx statements in
        Ok ({ span; statements } : block)
    | _ -> Error "")

and statement_of_json (ctx : of_json_ctx) (js : json) :
    (statement, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("span", span);
          ("id", id);
          ("kind", kind);
          ("comments_before", comments_before);
        ] ->
        let* span = span_of_json ctx span in
        let* statement_id = statement_id_of_json ctx id in
        let* kind = statement_kind_of_json ctx kind in
        let* comments_before =
          list_of_json string_of_json ctx comments_before
        in
        Ok ({ span; statement_id; kind; comments_before } : statement)
    | _ -> Error "")

and statement_id_of_json (ctx : of_json_ctx) (js : json) :
    (statement_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> StatementId.id_of_json ctx x
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
    | `Assoc [ ("Drop", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = place_of_json ctx x_0 in
        let* x_1 = trait_ref_of_json ctx x_1 in
        let* x_2 = drop_kind_of_json ctx x_2 in
        Ok (Drop (x_0, x_1, x_2))
    | `Assoc
        [
          ("Assert", `Assoc [ ("assert", assert_); ("on_failure", on_failure) ]);
        ] ->
        let* assert_ = assertion_of_json ctx assert_ in
        let* on_failure = abort_kind_of_json ctx on_failure in
        Ok (Assert (assert_, on_failure))
    | `Assoc [ ("Call", call) ] ->
        let* call = call_of_json ctx call in
        Ok (Call call)
    | `Assoc [ ("Abort", abort) ] ->
        let* abort = abort_kind_of_json ctx abort in
        Ok (Abort abort)
    | `String "Return" -> Ok Return
    | `Assoc [ ("Break", break) ] ->
        let* break = int_of_json ctx break in
        Ok (Break break)
    | `Assoc [ ("Continue", continue) ] ->
        let* continue = int_of_json ctx continue in
        Ok (Continue continue)
    | `String "Nop" -> Ok Nop
    | `Assoc [ ("Switch", switch) ] ->
        let* switch = switch_of_json ctx switch in
        Ok (Switch switch)
    | `Assoc [ ("Loop", loop) ] ->
        let* loop = block_of_json ctx loop in
        Ok (Loop loop)
    | `Assoc [ ("Error", error) ] ->
        let* error = string_of_json ctx error in
        Ok (Error error)
    | _ -> Error "")

and switch_of_json (ctx : of_json_ctx) (js : json) : (switch, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("If", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = operand_of_json ctx x_0 in
        let* x_1 = block_of_json ctx x_1 in
        let* x_2 = block_of_json ctx x_2 in
        Ok (If (x_0, x_1, x_2))
    | `Assoc [ ("SwitchInt", `List [ x_0; x_1; x_2; x_3 ]) ] ->
        let* x_0 = operand_of_json ctx x_0 in
        let* x_1 = literal_type_of_json ctx x_1 in
        let* x_2 =
          list_of_json
            (pair_of_json (list_of_json literal_of_json) block_of_json)
            ctx x_2
        in
        let* x_3 = block_of_json ctx x_3 in
        Ok (SwitchInt (x_0, x_1, x_2, x_3))
    | `Assoc [ ("Match", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = place_of_json ctx x_0 in
        let* x_1 =
          list_of_json
            (pair_of_json (list_of_json variant_id_of_json) block_of_json)
            ctx x_1
        in
        let* x_2 = option_of_json block_of_json ctx x_2 in
        Ok (Match (x_0, x_1, x_2))
    | _ -> Error "")
