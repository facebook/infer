(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module T = Textual
module S = StructuredIR

let pp_sir sir = F.printf "@[<v>%a@]@." S.pp sir

let pp_cfg sir =
  let nodes, _start = S.to_cfg sir in
  List.iter nodes ~f:(fun (node : T.Node.t) ->
      F.printf "#%a:@." T.NodeName.pp node.label ;
      List.iter node.instrs ~f:(fun instr ->
          F.printf "    %a@." (T.Instr.pp ~show_location:false) instr ) ;
      F.printf "    %a@." T.Terminator.pp node.last )


let mk_let name =
  T.Instr.Let
    { id= Some (T.Ident.of_int name)
    ; exp= T.Exp.Const (T.Const.Str (F.asprintf "action_%d" name))
    ; loc= T.Location.Unknown }


let nn s = T.NodeName.of_string s

let bexp0 = T.BoolExp.Exp (T.Exp.Var (T.Ident.of_int 0))

let sir_straight =
  S.Seq
    ( S.Instrs {label= nn "entry"; instrs= [mk_let 0]}
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 42))} )


let sir_diamond =
  S.Seq
    ( S.If
        { label= nn "cond"
        ; bexp= bexp0
        ; then_= S.Instrs {label= nn "then_"; instrs= [mk_let 1]}
        ; else_= S.Instrs {label= nn "else_"; instrs= [mk_let 2]} }
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 0))} )


let sir_loop =
  S.Seq
    ( S.Loop
        { label= nn "loop"
        ; body=
            S.Seq
              ( S.Instrs {label= nn "body"; instrs= [mk_let 0]}
              , S.If
                  { label= nn "test"
                  ; bexp= bexp0
                  ; then_= S.Branch 0
                  ; else_= S.Seq (S.Instrs {label= nn "cont"; instrs= [mk_let 1]}, S.Branch 1) } )
        }
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 0))} )


let pp_rpo nodes start =
  let rpo = S.reverse_postorder nodes start in
  let sorted =
    T.NodeName.Map.bindings rpo |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  in
  List.iter sorted ~f:(fun (name, n) -> F.printf "%d: %a@." n T.NodeName.pp name)


let%test_module "structured IR" =
  ( module struct
    let%expect_test "straight-line pp" =
      pp_sir sir_straight ;
      [%expect {|
        n0 = "action_0"
        ret 42 |}]


    let%expect_test "straight-line to_cfg" =
      pp_cfg sir_straight ;
      [%expect
        {|
        #entry:
            n0 = "action_0"
            jmp ret
        #ret:
            ret 42
        |}]


    let%expect_test "diamond pp" =
      pp_sir sir_diamond ;
      [%expect
        {|
        if n0 then
          n1 = "action_1"

        else
          n2 = "action_2"

        ret 0
        |}]


    let%expect_test "diamond to_cfg" =
      pp_cfg sir_diamond ;
      [%expect
        {|
        #cond:
            if n0 then jmp then_ else jmp else_
        #then_:
            n1 = "action_1"
            jmp ret
        #else_:
            n2 = "action_2"
            jmp ret
        #ret:
            ret 0
        |}]


    let%expect_test "loop pp" =
      pp_sir sir_loop ;
      [%expect
        {|
        loop
          n0 = "action_0"
          if n0 then
            branch 0

          else
            n1 = "action_1"
            branch 1


        ret 0
        |}]


    let%expect_test "loop to_cfg" =
      pp_cfg sir_loop ;
      [%expect
        {|
        #loop:
            jmp body
        #body:
            n0 = "action_0"
            jmp test
        #test:
            if n0 then jmp ret else jmp cont
        #cont:
            n1 = "action_1"
            jmp loop
        #ret:
            ret 0
        |}]


    let%expect_test "RPO straight-line" =
      let nodes, start = S.to_cfg sir_straight in
      pp_rpo nodes start ;
      [%expect {|
        0: entry
        1: ret
        |}]


    let%expect_test "RPO diamond" =
      let nodes, start = S.to_cfg sir_diamond in
      pp_rpo nodes start ;
      [%expect {|
        0: cond
        1: else_
        2: then_
        3: ret
        |}]


    let%expect_test "RPO loop" =
      let nodes, start = S.to_cfg sir_loop in
      pp_rpo nodes start ;
      [%expect
        {|
        0: loop
        1: body
        2: test
        3: cont
        4: ret
        |}]
  end )
