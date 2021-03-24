/*===-- ipo_ocaml.c - LLVM OCaml Glue ---------------------------*- C++ -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This file glues LLVM's OCaml interface to its C interface. These functions *|
|* are by and large transparent wrappers to the corresponding C functions.    *|
|*                                                                            *|
|* Note that these functions intentionally take liberties with the CAMLparamX *|
|* macros, since most of the parameters are not GC heap objects.              *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#include <assert.h>
#include "llvm-c/Core.h"
#include "llvm-c/Transforms/IPO.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_argument_promotion(LLVMPassManagerRef PM) {
  LLVMAddArgumentPromotionPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_constant_merge(LLVMPassManagerRef PM) {
  LLVMAddConstantMergePass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_merge_functions(LLVMPassManagerRef PM) {
  LLVMAddMergeFunctionsPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_dead_arg_elimination(LLVMPassManagerRef PM) {
  LLVMAddDeadArgEliminationPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_function_attrs(LLVMPassManagerRef PM) {
  LLVMAddFunctionAttrsPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_function_inlining(LLVMPassManagerRef PM) {
  LLVMAddFunctionInliningPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_always_inliner(LLVMPassManagerRef PM) {
  LLVMAddAlwaysInlinerPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_global_dce(LLVMPassManagerRef PM) {
  LLVMAddGlobalDCEPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_global_optimizer(LLVMPassManagerRef PM) {
  LLVMAddGlobalOptimizerPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_ip_constant_propagation(LLVMPassManagerRef PM) {
  LLVMAddIPConstantPropagationPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_prune_eh(LLVMPassManagerRef PM) {
  LLVMAddPruneEHPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_ipsccp(LLVMPassManagerRef PM) {
  LLVMAddIPSCCPPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> all_but_main:bool -> unit */
CAMLprim value llvm_add_internalize(LLVMPassManagerRef PM, value AllButMain) {
  LLVMAddInternalizePass(PM, Bool_val(AllButMain));
  return Val_unit;
}

/* string -> bool */
static value *predicate_f = NULL;

LLVMBool MustPreserveCallBack(LLVMValueRef Val, void* Ctx) {
  CAMLparam0();
  CAMLlocal1(LLVMValName);
  const char *llvmValName;
  LLVMBool ret;
  assert(predicate_f != NULL
         && "llvm_add_internalize_predicate must be called with \
             LLVMInternalizePredicateCallback symbol set");

  llvmValName = LLVMGetValueName(Val);
  LLVMValName = caml_copy_string(llvmValName);
  ret = Bool_val(caml_callback(*predicate_f, LLVMValName));

  CAMLreturnT(LLVMBool, ret);
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_internalize_predicate(LLVMPassManagerRef PM) {
  predicate_f = caml_named_value("LLVMInternalizePredicateCallback");
  LLVMAddInternalizePassWithMustPreservePredicate(PM, NULL,
                                                  &MustPreserveCallBack);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_strip_dead_prototypes(LLVMPassManagerRef PM) {
  LLVMAddStripDeadPrototypesPass(PM);
  return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_strip_symbols(LLVMPassManagerRef PM) {
  LLVMAddStripSymbolsPass(PM);
  return Val_unit;
}
