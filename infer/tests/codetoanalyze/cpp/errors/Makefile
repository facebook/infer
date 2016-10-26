# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

OPTIONS = -x c++ -std=c++11 -isystem$(ROOT_DIR) -isystem$(CLANG_INCLUDES)/c++/v1/ -c

FILES = \
  c_tests/*.cpp \
  include_header/header.h \
  include_header/include_templ.cpp \
  memory_leaks/*.cpp \
  models/*.cpp \
  npe/*.cpp \
  numeric/*.cpp \
  overwrite_attribute/*.cpp \
  resource_leaks/*.cpp \
  shared/attributes/*.cpp \
  shared/conditional/*.cpp \
  shared/constructors/constructor_init.cpp \
  shared/constructors/constructor_new.cpp \
  shared/constructors/constructor_with_body.cpp \
  shared/constructors/copy_move_constructor.cpp \
  shared/constructors/temp_object.cpp \
  shared/exceptions/Exceptions.cpp \
  shared/lambda/*.cpp \
  shared/methods/conversion_operator.cpp \
  shared/methods/static.cpp \
  shared/namespace/function.cpp \
  shared/namespace/global_variable.cpp \
  shared/methods/virtual_methods.cpp \
  shared/nestedoperators/var_decl_inside_if.cpp \
  shared/npe/*.cpp \
  shared/reference/reference_field.cpp \
  shared/reference/reference_struct_e2e.cpp \
  shared/reference/reference_type_e2e.cpp \
  shared/reference/temporary_lvalue.cpp \
  shared/templates/class_template_instantiate.cpp \
  shared/templates/function.cpp \
  shared/templates/function_pack.cpp \
  shared/templates/method.cpp \
  shared/types/inheritance_field.cpp \
  shared/types/operator_overload.cpp \
  shared/types/return_struct.cpp \
  shared/types/struct_forward_declare.cpp \
  shared/types/struct_pass_by_value.cpp \
  shared/types/typeid_expr.cpp \
  smart_ptr/*.cpp \
  subtyping/*.cpp \
  vector/*.cpp \

compile:
	clang $(OPTIONS) $(FILES)

analyze:
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cpp --check-duplicate-symbols -- clang $(OPTIONS) $(FILES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
