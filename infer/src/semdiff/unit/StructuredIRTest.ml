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


(* nested loops + multiple ifs:
   outer_loop { a0; if c { a1; inner_loop { a2; if c then break else { a3; continue } }; a4; continue_outer } else { a5; break_outer } }; ret *)
let sir_nested =
  S.Seq
    ( S.Loop
        { label= nn "outer"
        ; body=
            S.Seq
              ( S.Instrs {label= nn "a0"; instrs= [mk_let 0]}
              , S.If
                  { label= nn "t0"
                  ; bexp= bexp0
                  ; then_=
                      S.Seq
                        ( S.Instrs {label= nn "a1"; instrs= [mk_let 1]}
                        , S.Seq
                            ( S.Loop
                                { label= nn "inner"
                                ; body=
                                    S.Seq
                                      ( S.Instrs {label= nn "a2"; instrs= [mk_let 2]}
                                      , S.If
                                          { label= nn "t1"
                                          ; bexp= bexp0
                                          ; then_= S.Branch 0
                                          ; else_=
                                              S.Seq
                                                ( S.Instrs {label= nn "a3"; instrs= [mk_let 3]}
                                                , S.Branch 1 ) } ) }
                            , S.Seq (S.Instrs {label= nn "a4"; instrs= [mk_let 4]}, S.Branch 1) ) )
                  ; else_= S.Instrs {label= nn "a5"; instrs= [mk_let 5]} } ) }
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 0))} )


let pp_rpo nodes start =
  let rpo = S.reverse_postorder nodes start in
  let sorted =
    T.NodeName.Map.bindings rpo |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  in
  List.iter sorted ~f:(fun (name, n) -> F.printf "%d: %a@." n T.NodeName.pp name)


let pp_domtree nodes start =
  let idom = S.compute_idom nodes start in
  let children = S.dominator_children idom in
  let rpo = S.reverse_postorder nodes start in
  let sort_by_rpo names =
    List.sort names ~compare:(fun a b ->
        Int.compare (T.NodeName.Map.find a rpo) (T.NodeName.Map.find b rpo) )
  in
  let rec pp_children ~prefix kids =
    let n = List.length kids in
    List.iteri kids ~f:(fun i kid ->
        let is_last = Int.equal i (n - 1) in
        let connector = if is_last then "└── " else "├── " in
        let child_prefix = prefix ^ if is_last then "    " else "│   " in
        F.printf "%s%s%a@." prefix connector T.NodeName.pp kid ;
        let grandkids =
          sort_by_rpo (T.NodeName.Map.find_opt kid children |> Option.value ~default:[])
        in
        pp_children ~prefix:child_prefix grandkids )
  in
  F.printf "%a@." T.NodeName.pp start ;
  let kids = sort_by_rpo (T.NodeName.Map.find_opt start children |> Option.value ~default:[]) in
  pp_children ~prefix:"" kids


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


    let%expect_test "domtree straight-line" =
      let nodes, start = S.to_cfg sir_straight in
      pp_domtree nodes start ;
      [%expect {|
        entry
        └── ret
        |}]


    let%expect_test "domtree diamond" =
      let nodes, start = S.to_cfg sir_diamond in
      pp_domtree nodes start ;
      [%expect {|
        cond
        ├── else_
        ├── then_
        └── ret
        |}]


    let%expect_test "domtree loop" =
      let nodes, start = S.to_cfg sir_loop in
      pp_domtree nodes start ;
      [%expect
        {|
        loop
        └── body
            └── test
                ├── cont
                └── ret
        |}]


    let%expect_test "nested pp" =
      pp_sir sir_nested ;
      [%expect
        {|
        loop
          n0 = "action_0"
          if n0 then
            n1 = "action_1"
            loop
              n2 = "action_2"
              if n0 then
                branch 0

              else
                n3 = "action_3"
                branch 1


            n4 = "action_4"
            branch 1

          else
            n5 = "action_5"


        ret 0
        |}]


    let%expect_test "nested to_cfg" =
      pp_cfg sir_nested ;
      [%expect
        {|
        #outer:
            jmp a0
        #a0:
            n0 = "action_0"
            jmp t0
        #t0:
            if n0 then jmp a1 else jmp a5
        #a1:
            n1 = "action_1"
            jmp inner
        #inner:
            jmp a2
        #a2:
            n2 = "action_2"
            jmp t1
        #t1:
            if n0 then jmp a4 else jmp a3
        #a3:
            n3 = "action_3"
            jmp inner
        #a4:
            n4 = "action_4"
            jmp outer
        #a5:
            n5 = "action_5"
            jmp ret
        #ret:
            ret 0
        |}]


    let%expect_test "domtree nested" =
      let nodes, start = S.to_cfg sir_nested in
      pp_domtree nodes start ;
      [%expect
        {|
        outer
        └── a0
            └── t0
                ├── a5
                │   └── ret
                └── a1
                    └── inner
                        └── a2
                            └── t1
                                ├── a3
                                └── a4
        |}]
  end )
