; Copyright (c) 2015 - present Facebook, Inc.
; All rights reserved.
;
; This source code is licensed under the BSD style license found in the
; LICENSE file in the root directory of this source tree. An additional grant
; of patent rights can be found in the PATENTS file in the same directory.

; ModuleID = 'null_deref.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.10.0"

; Function Attrs: nounwind ssp uwtable
define void @foo() #0 {
  %p = alloca i32*, align 8
  call void @llvm.dbg.declare(metadata !{i32** %p}, metadata !11), !dbg !14
  store i32* null, i32** %p, align 8, !dbg !15
  %1 = load i32** %p, align 8, !dbg !16
  store i32 42, i32* %1, align 4, !dbg !16
  ret void, !dbg !17
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!8, !9}
!llvm.ident = !{!10}

!0 = metadata !{i32 786449, metadata !1, i32 12, metadata !"Apple LLVM version 6.1.0 (clang-602.0.53) (based on LLVM 3.6.0svn)", i1 false, metadata !"", i32 0, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"", i32 1} ; [ DW_TAG_compile_unit ] [/Users/rohanjr/infer/infer/tests/codetoanalyze/llvm/null_deref.c] [DW_LANG_C99]
!1 = metadata !{metadata !"null_deref.c", metadata !"/Users/rohanjr/infer/infer/tests/codetoanalyze/llvm"}
!2 = metadata !{}
!3 = metadata !{metadata !4}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"foo", metadata !"foo", metadata !"", i32 10, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, void ()* @foo, null, null, metadata !2, i32 10} ; [ DW_TAG_subprogram ] [line 10] [def] [foo]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/rohanjr/infer/infer/tests/codetoanalyze/llvm/null_deref.c]
!6 = metadata !{i32 786453, i32 0, null, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, null, null, null} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{null}
!8 = metadata !{i32 2, metadata !"Dwarf Version", i32 2}
!9 = metadata !{i32 2, metadata !"Debug Info Version", i32 602053001}
!10 = metadata !{metadata !"Apple LLVM version 6.1.0 (clang-602.0.53) (based on LLVM 3.6.0svn)"}
!11 = metadata !{i32 786688, metadata !4, metadata !"p", metadata !5, i32 11, metadata !12, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [p] [line 11]
!12 = metadata !{i32 786447, null, null, metadata !"", i32 0, i64 64, i64 64, i64 0, i32 0, metadata !13} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from int]
!13 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 11, i32 9, metadata !4, null} ; [ DW_TAG_lexical_block ] [/]
!15 = metadata !{i32 11, i32 4, metadata !4, null} ; [ DW_TAG_lexical_block ] [/]
!16 = metadata !{i32 12, i32 4, metadata !4, null}
!17 = metadata !{i32 13, i32 1, metadata !4, null}
