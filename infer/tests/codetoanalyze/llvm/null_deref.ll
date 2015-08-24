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
  call void @llvm.dbg.declare(metadata i32** %p, metadata !12, metadata !15), !dbg !16
  store i32* null, i32** %p, align 8, !dbg !16
  %1 = load i32** %p, align 8, !dbg !17
  store i32 42, i32* %1, align 4, !dbg !18
  ret void, !dbg !19
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!8, !9, !10}
!llvm.ident = !{!11}

!0 = !{!"0x11\0012\00clang version 3.6.1 (tags/RELEASE_361/final)\000\00\000\00\001", !1, !2, !2, !3, !2, !2} ; [ DW_TAG_compile_unit ] [/Users/rohanjr/infer/infer/tests/codetoanalyze/llvm/null_deref.c] [DW_LANG_C99]
!1 = !{!"null_deref.c", !"/Users/rohanjr/infer/infer/tests/codetoanalyze/llvm"}
!2 = !{}
!3 = !{!4}
!4 = !{!"0x2e\00foo\00foo\00\0010\000\001\000\000\00256\000\0010", !1, !5, !6, null, void ()* @foo, null, null, !2} ; [ DW_TAG_subprogram ] [line 10] [def] [foo]
!5 = !{!"0x29", !1}                               ; [ DW_TAG_file_type ] [/Users/rohanjr/infer/infer/tests/codetoanalyze/llvm/null_deref.c]
!6 = !{!"0x15\00\000\000\000\000\000\000", null, null, null, !7, null, null, null} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = !{null}
!8 = !{i32 2, !"Dwarf Version", i32 2}
!9 = !{i32 2, !"Debug Info Version", i32 2}
!10 = !{i32 1, !"PIC Level", i32 2}
!11 = !{!"clang version 3.6.1 (tags/RELEASE_361/final)"}
!12 = !{!"0x100\00p\0011\000", !4, !5, !13}       ; [ DW_TAG_auto_variable ] [p] [line 11]
!13 = !{!"0xf\00\000\0064\0064\000\000", null, null, !14} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from int]
!14 = !{!"0x24\00int\000\0032\0032\000\000\005", null, null} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!15 = !{!"0x102"}                                 ; [ DW_TAG_expression ]
!16 = !MDLocation(line: 11, column: 9, scope: !4)
!17 = !MDLocation(line: 12, column: 5, scope: !4)
!18 = !MDLocation(line: 12, column: 4, scope: !4)
!19 = !MDLocation(line: 13, column: 1, scope: !4)
