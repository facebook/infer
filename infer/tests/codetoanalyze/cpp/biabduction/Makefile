# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# use our own clang's standard library so that the tests are uniform across distributions
CLANG_OPTIONS = -x c++ -std=c++1y -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = \
  --biabduction-only --biabduction-memleak-buckets cpp --debug-exceptions --report-custom-error \
  --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard biabduction/*.cpp) \
  $(wildcard c_tests/*.cpp) \
  include_header/header.h \
  include_header/include_templ.cpp \
  $(wildcard memory_leaks/*.cpp) \
  $(wildcard models/*.cpp) \
  $(wildcard generic_models/*.cpp) \
  $(wildcard mutex/*.cpp) \
  $(wildcard npe/*.cpp) \
  $(wildcard numeric/*.cpp) \
  $(wildcard overwrite_attribute/*.cpp) \
  $(wildcard pointers/*.cpp) \
  $(wildcard resource_leaks/*.cpp) \
  $(wildcard shared/attributes/*.cpp) \
  $(wildcard shared/conditional/*.cpp) \
  shared/constructors/constructor_init.cpp \
  shared/constructors/constructor_new.cpp \
  shared/constructors/constructor_with_body.cpp \
  shared/constructors/copy_move_constructor.cpp \
  shared/constructors/temp_object.cpp \
  shared/constructors/copy_array_field.cpp \
  shared/exceptions/Exceptions.cpp \
  $(wildcard shared/lambda/*.cpp) \
  shared/methods/conversion_operator.cpp \
  shared/methods/static.cpp \
  shared/namespace/function.cpp \
  shared/namespace/global_variable.cpp \
  shared/methods/virtual_methods.cpp \
  shared/nestedoperators/var_decl_inside_if.cpp \
  $(wildcard shared/npe/*.cpp) \
  shared/reference/reference_field.cpp \
  shared/reference/reference_struct_e2e.cpp \
  shared/reference/reference_type_e2e.cpp \
  shared/reference/temporary_lvalue.cpp \
  shared/templates/class_template_instantiate.cpp \
  shared/templates/class_specialization.cpp \
  shared/templates/function.cpp \
  shared/templates/function_pack.cpp \
  shared/templates/method.cpp \
  shared/types/const.cpp \
  shared/types/inheritance_casts.cpp \
  shared/types/inheritance_field.cpp \
  shared/types/operator_overload.cpp \
  shared/types/return_struct.cpp \
  shared/types/struct_forward_declare.cpp \
  shared/types/struct_pass_by_value.cpp \
  $(wildcard smart_ptr/*.cpp) \
  $(wildcard stack_escape/*.cpp) \
  $(wildcard static_local/*.cpp) \
  $(wildcard subtyping/*.cpp) \
  $(wildcard templates/*.cpp) \
  $(wildcard types/*.cpp) \
  $(wildcard use_after_free/*.cpp) \
  $(wildcard vector/*.cpp) \

include $(TESTS_DIR)/clang.make
