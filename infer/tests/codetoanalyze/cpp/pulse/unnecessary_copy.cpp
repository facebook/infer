/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>
#include <assert.h>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "header.h"

struct Arr {
  int arr[2];
  std::vector<int> vec;
};

Arr& get_a_ref() {
  static Arr a;
  return a;
}

auto global = get_a_ref();

int copy_decl_bad() {
  auto a = get_a_ref(); // unnecessary copy, use a ref
  // call to copy constructor Arr::Arr(a, n$0)
  return a.arr[0];
}

int source_mod_ok() {
  Arr source;
  auto cpy = source;
  source.arr[0] = 9; // source is modified, so copy is not unnecessary as we
                     // can't just add &
  return cpy.arr[0];
}

int source_mod_param_ok(Arr source) {
  auto cpy = source;
  source.arr[0] = 9; // source is modified, so copy is not unnecessary as we
                     // can't just add &
  return cpy.arr[0];
}

void copy_in_branch_bad(bool b) {
  if (b) {
    Arr a;
    auto cpy = a;
  }
}

void copy_in_branch_mod_ok(bool b) {
  if (b) {
    Arr a;
    auto cpy = a;
    cpy.arr[0] = 8;
  }
}

void copy_outside_branch_mod_ok(bool b) {
  Arr a;
  auto cpy = a;
  if (b) {
    cpy.arr[0] = 8;
  }
}

void multiple_copies_bad(bool b) {
  Arr a;
  auto cpy1 = a; // unnecessary copy
  if (b) {
    auto cpy2 = cpy1; // unnecessary copy
  }
}

void modified_copy_decl_ok() {
  auto a = get_a_ref();
  a.arr[0] = 9; // copy modified here, it is not unnecessary
}

// Clang frontend doesn't properly translate array copy constructor
void copy_via_constructor_bad_FN() {
  auto my_arr = Arr{{1, 2}}; // call to constructor
  auto c = my_arr; // copy
  c.arr[0] = 9;
}

int get_first_elem(Arr a) { return a.arr[0]; }

void copy_via_constructor_read_bad() {
  auto my_arr = Arr{{1, 2}}; // call to constructor
  auto cpy_arr = my_arr; // copy
  get_first_elem(cpy_arr);
}

Arr get_a() {
  Arr a;
  return a;
}

void set_to_zero(int arr[]) { arr[0] = 0; }

void modified_interproc_copy_decl_ok() {
  auto a = get_a_ref();
  set_to_zero(a.arr); // copy modified here by callee, it is not unnecessary
}

void copy_vec_bad(std::vector<int> vec) { auto cpy_vec = vec; }

void copy_vec_mod_ok(std::vector<int> vec) {
  auto cpy_vec = vec;
  cpy_vec.push_back(0);
}

class Vec {

 public:
  std::vector<int> vec;
  std::list<Arr> my_list;

  Vec() {
    for (int i = 1; i <= 3; i++) {
      vec.push_back(i);
    }
  }

  Vec(const Vec& v) {
    for (int i = 1; i <= 3; i++)
      vec.push_back(v.get(i));
  }

  void setVec(std::vector<int> my_vec) { vec = std::move(my_vec); }

  void intermediate_field_copy_ok() { setVec(vec); }

  int get(int i) const { return vec[i]; }

  void source_modified_via_unmodeled_ok() {
    auto arr = my_list.front(); // result of unknown call on source is copied
    my_list.pop_front();
    // when checking for modifications, we need to check that arr is propagated
    // from my_list which is modified
  }
};

void copy_own_vec_bad() {
  Vec vec;
  auto copied_own_vec = vec; // copy
}

// copy created only in one branch
void loop_via_copy_bad(std::vector<std::string>& namesOfTheEntirePopulation) {
  for (const auto name :
       namesOfTheEntirePopulation) { // copies the name unnecessarily
                                     // use const auto&
  }
}

// as opposed to the above one, there is no copy here
void loop_no_copy_ok(std::vector<std::string>& namesOfTheEntirePopulation) {
  for (const auto& name : namesOfTheEntirePopulation) {
  }
}

Arr& get_cond_arr_ref(Arr& arr1, Arr& arr2, bool cond) {
  if (cond) {
    return arr1;
  } else {
    return arr2;
  }
}

void copy_in_both_cases_bad(bool cond) {
  Arr arr1;
  Arr arr2;
  auto cpy = get_cond_arr_ref(arr1, arr2, cond); // call to copy ctor
}

// We can't detect this case because we only keep track of one source
// (arr's abstract value) but in two brances they point to two
// different addresses, making us think that one of them is modified
void copy_in_both_cases_aliasing_bad_FN(bool cond) {
  Arr arr;
  auto cpy = get_cond_arr_ref(arr, arr, cond); // call to copy ctor
}

void copy_in_both_cases_mod_ok(bool cond) {
  Arr arr1;
  Arr arr2;
  auto cpy = get_cond_arr_ref(arr1, arr2, cond); // call to copy ctor
  cpy.arr[0] = 9;
}

void copy_in_both_cases_source_mod_ok(bool cond) {
  Arr arr1;
  Arr arr2;
  auto cpy = get_cond_arr_ref(arr1, arr2, cond); // call to copy ctor
  arr1.arr[0] = 9;
}

void copy_in_both_cases_branch_bad(bool cond) {
  Arr arr1;
  Arr arr2;
  auto cpy = get_cond_arr_ref(arr1, arr2, true); // call to copy ctor
}

void copy_modified_after_abort_ok(std::vector<int> source_vec) {
  auto cpy = source_vec;
  std::vector<int> vec(2);
  int* elt = &vec[1];
  vec.push_back(0);
  int temp = *elt; // abort: vector invalidation, so non-disjunctive
                   // value becomes top
  cpy.push_back(0);
}

namespace ns {

template <typename X>
X creates_copy(X a) {
  return X{a};
}
} // namespace ns

void copy_via_model_bad(Arr arr) {
  auto cpy = ns::creates_copy(arr); // creates copy (via model)
}

void source_modified_before_lib_destructor_ok(std::vector<int>& source_vec) {
  auto cpy = source_vec;
  source_vec[0] = 0;
}

void copy_modified_before_lib_destructor_ok(std::vector<int>& source_vec) {
  auto cpy = source_vec;
  cpy[0] = 0;
}

class String {
 private:
  char* text;
  int size;

 public:
  int x;
  ~String() { delete[] text; } // destructor
  void set_size(int new_size) { size = new_size; }
};

void check_before_custom_destructor_bad(String s) { auto cpy = s; }

void modified_before_custom_destructor_ok(String s) {
  auto cpy = s;
  cpy.set_size(10);
}

int get_size(String s) { return s.x; }
void check_no_move_op_bad(String arg) {
  int x = get_size(std::move(arg)); // String has no auto-generated move
                                    // constructor so there is no move here!
}

void copy_vec_name_contains_copy_ok(std::vector<int> vec) {
  auto copy_vec = vec;
} // variable contains "copy", hence warning should be suppressed

void set_erase_ok(std::set<int> source) {
  auto cpy = source;
  cpy.erase(3); // unmodeled call assumes all non-const ptr args are modified
}

void source_const_fcn_bad(std::set<int> source) {
  auto cpy = source;
  int count =
      source.count(3); // count is a const function which doesn't modify this*
}

void source_dispatch_const_fcn_bad(const std::set<int>& source) {
  auto cpy = source;
  auto it = source.find(3); // const find is dispatched since argument is const
}

void source_dispatch_non_const_fcn_bad(std::set<int> source) {
  auto cpy = source;
  auto it = source.find(3);
}

void iterator_ptr_modified_ok(const std::vector<int>& numbers) {
  auto lDataValues = numbers;
  std::sort(lDataValues.begin(), lDataValues.end());
}

struct SimpleS {
  int a;
  std::vector<int> vec;
};

struct SwapSimple {
  SimpleS v;
  void swap_bad_FN(SwapSimple& x) {
    const auto temp = v;
    v = x.v;
    x.v = temp; // report copy assignment from const
  }
};

struct SwapVector {
  std::vector<int> v;
  void swap_bad_FN(SwapVector& x) {
    const auto temp = v;
    v = x.v;
    x.v = temp; // report copy assignment from const
  }
};

void capture_by_value_ok(SimpleS arg) {
  auto f = [c = arg]() mutable { c.a = 19; };
}

// NOTE: Currently we do not support unnecessary capture-by-value in lambda.
void capture_by_value_bad_FN(SimpleS arg) {
  auto f = [c = arg]() { int n = c.a; };
}

void constructor_bad() {
  std::vector<int> source;
  auto cpy = source;
}

// We can't detect this due to aliasing problem when we analyze the source code
// of shared ptr copy ctor
void shared_ptr_bad_FN(std::shared_ptr<Arr> source) { auto c = source; }

void copy_assignment_bad(std::set<int> source) {
  std::set<int> init_set; // default constructor is called
  init_set = source; // copy assignment operator is called
}

void copy_assignment_ok(std::set<int> source) {
  std::set<int> init_set; // default constructor is called
  init_set = source; // copy assignment operator is called
  source.insert(1); // source modified
}

void move_assignment_ok(std::set<int> source) {
  std::set<int> init_set; // default constructor is called
  init_set =
      std::move(source); // move assignment operator is called, no copy created
}

void get_rvalue_ref(std::set<int>&& x) {}

void copy_and_move_bad(std::set<int> source) {
  std::set<int> c = source;
  get_rvalue_ref(std::move(c)); // We can move source without copy.
}

void copy_and_move_const_ref_ok(const std::set<int>& source) {
  std::set<int> c = source;
  get_rvalue_ref(std::move(c));
}

struct TriviallyCopyable {
  int a;
  float f;
  int* p;
};

void copy_trivially_copyable_ok(TriviallyCopyable source) {
  TriviallyCopyable c = source;
}

class WrapperArr {

  explicit WrapperArr(const Arr& internal_arr) : hidden_arr_(internal_arr) {}

  // unnecessary copy into hidden_arr_, it should be moved
  explicit WrapperArr(Arr&& internal_arr) : hidden_arr_(internal_arr) {}

  const Arr getArr() const { return hidden_arr_; }

 private:
  Arr hidden_arr_;

  void unnecessary_copy_moveable_bad(Arr&& a) {
    hidden_arr_ = a;
    hidden_arr_.arr[0] = 9; // it is ok that the copy is modified since it has
                            // the ownership of the object.
  }

  void unnecessary_copy_moveable_source_mod_ok(Arr&& a) {
    hidden_arr_ = a;
    a.arr[0] = 9; // we cannot suggest move above as source is modified
  }

  void unnecessary_copy_moveable_copy_mod_bad(Arr&& a) {
    hidden_arr_ = a;
    hidden_arr_.arr[0] = 9; // copy can be modified since it will have the
                            // ownership of the object.
  }

  void copy_assignment_from_this_ok() {
    Arr local_arr; // default constructor is called
    local_arr = hidden_arr_; // copy assignment operator is called but it is
                             // from a member field which cannot be moved.
  }
};

namespace my_proj {
struct CheapToCopy {
  std::vector<int> vec;
};

void cheap_to_copy_ok(CheapToCopy source) { auto c = source; }
}; // namespace my_proj

void unnecessary_copy_initializer_list(std::vector<int> c1,
                                       std::vector<int> c2) {
  for (const auto& c : {c1, c2}) {
  }
  // fix here is not to add & but use ptrs
  // for (const auto* c : { &c1, &c2 }) // use *c
}

class LockedPtr {};

class MyValueOr {
  bool b;
  Arr& value;
  std::shared_ptr<Arr> shared_ptr;
  LockedPtr lock();
  LockedPtr rlock();

 public:
  MyValueOr();

  Arr value_or(const Arr& default_value) const {
    if (b) {
      return value;
    } else {
      return default_value;
    }
  }

  Arr get_arr_implicit_cpy() const { return get_a_ref(); }

  std::shared_ptr<Arr> cpy_shared_ptr() const { return shared_ptr; }

  Arr intentional_copy() const { return get_a_ref(); }

  Arr intentional_cpy_under_lock() {
    auto l = lock();
    return value;
  }

  Arr intentional_cpy_under_rlock_ok() {
    auto l = rlock();
    auto result = value;
    return result;
  }

  Arr no_cpy_NRVO() const {
    Arr x;
    return x;
  }

  void suppress_intentional_cpy_under_lock(Arr src) {
    auto l = lock();
    auto tgt = src;
  }
};

void call_value_or_bad(const MyValueOr& c) {
  const static Arr f{};
  Arr g = c.value_or(f);
}

void call_value_or_ok(const MyValueOr& c) {
  const static Arr f{};
  Arr g = c.value_or(f);
  g.arr[0] = 42;
}

void call_get_arr_implicit_cpy_bad(const MyValueOr& c) {
  Arr g = c.get_arr_implicit_cpy();
}

void call_cpy_shared_ptr_ok(const MyValueOr& c) { auto g = c.cpy_shared_ptr(); }

void call_intentional_copy_ok(const MyValueOr& c) {
  auto g = c.intentional_copy();
}

void call_intentional_cpy_under_lock_ok(MyValueOr c) {
  auto g = c.intentional_cpy_under_lock();
}

void call_no_cpy_NRVO_ok(const MyValueOr& c) { auto g = c.no_cpy_NRVO(); }

class ClassWithoutConstructDef {
  int __internal_field;
  std::vector<int> __internal_vec;

  // we should detect copy assignment and suggest a move into the field
  void field_setter_bad(std::vector<int> vec) {
    __internal_vec = vec; // copy assignment
  }

 public:
  ClassWithoutConstructDef(const ClassWithoutConstructDef& src);
  int* get_field_ref() { return &__internal_field; }
};
void assign_value_unknown(int* ref, int v);

void modify_by_unknown_ok(const ClassWithoutConstructDef& src) {
  ClassWithoutConstructDef tgt = src;
  assign_value_unknown(tgt.get_field_ref(), 42);
}

void call_unknown_constructor_twice_ok(const ClassWithoutConstructDef& src) {
  ClassWithoutConstructDef tgt = src;
  assign_value_unknown(tgt.get_field_ref(), 42);
  ClassWithoutConstructDef dummy = tgt;
  assign_value_unknown(dummy.get_field_ref(), 42);
}

#define LOCAL_MACRO(accessor) \
  { auto cpy = (accessor); }

void foo(std::vector<int> my_vec) { LOCAL_MACRO(my_vec); }

class CopiedToField1_Bad {
  Arr field;

 public:
  CopiedToField1_Bad(Arr a) : field(a) {}
};

class CopiedToField1_Ok {
  Arr field;

 public:
  CopiedToField1_Ok(Arr a) : field(std::move(a)) {}
};

class CopiedToField2_Bad {
  Arr field;

 public:
  CopiedToField2_Bad(Arr a) { field = a; }
};

class CopiedToField2_Ok {
  Arr field;

 public:
  CopiedToField2_Ok(Arr a) { field = std::move(a); }
};

struct Arrs {
  Arr a;
  Arr b;
};

class CopiedToField3_Last_Bad {
  Arrs field1;
  Arr field2;

 public:
  // last copy could be avoided
  CopiedToField3_Last_Bad(Arrs as) {
    field1 = as;
    field2 = as.a;
  }
};

class PassedToUnknown_Bad {
  Arr field;

  static void unknown(Arr a);

 public:
  PassedToUnknown_Bad(Arr a) { unknown(a); }
};

class PassedToUnknown_Ok {
  Arr field;

  static void unknown(Arr a);

 public:
  PassedToUnknown_Ok(Arr a) { unknown(std::move(a)); }
};

class PassedToUnknownRef_Bad {
  Arr field;

  static void unknown(const Arr& a);

 public:
  // Ideally, the parameter can be changed to const-ref, but this pattern is not
  // common in practice. The test is just for showing checker's behavior.
  PassedToUnknownRef_Bad(Arr a) { unknown(a); }
};

class CopiedToMultipleField_Last_Bad {
  Arr field1;
  Arr field2;

 public:
  // Ideally, the last copy can be avoided by std::move, but this pattern is not
  // common in practice. The test is just for showing checker's behavior.
  CopiedToMultipleField_Last_Bad(Arr a) : field1(a), field2(a) {}
};

void global_setter_ok(const Arr& arr) {
  global = arr; // don't suggest std::move(arr) due to const
}

void modify_arg(std::vector<int> arg) { arg.push_back(42); }

void intermediate_copy_modified_unused_bad(std::vector<int> input) {
  modify_arg(input); // copy from input to an intermediate which is modified
  // input is never used so it is ok to suggest move
}

int intermediate_copy_modified_used_ok(std::vector<int> input) {
  modify_arg(input); // copy from input to an intermediate which is modified
  return input.size(); // input is used, we can't trivially suggest move without
                       // also moving its uses to before the copy is made which
                       // might hurt performance if accesses are conditional.
                       // Better don't report.
}

void intermediate_copy_modified_local_unused_bad() {
  std::vector<int> input = {0};
  input.push_back(1);
  modify_arg(input); // copy from input to an intermediate which is modified
  // input is never used so it is ok to suggest move
}

int intermediate_copy_modified_local_used_ok() {
  std::vector<int> input = {0};
  modify_arg(input); // copy from input to an intermediate which is modified
  return input[0];
}

int intermediate_local_copy_used_ok() {
  Arr input;
  int x = get_first_elem(input); // copy from input to an intermediate
  input.arr[0] = 0; // can't suggest moving input as it is used
  return x;
}

void copy_assignment_const_ref_member_ok(const Arr& arr) {
  std::vector<int> my_vec;
  my_vec = arr.vec;
}

void copy_assignment_const_value_param_bad(const Arr arr) {
  Arr my_arr;
  my_arr = arr; // fix here is to remove const from param type
}

void copy_assignment_const_ref_param_ok(const Arr& arr) {
  Arr my_arr;
  const Arr& c = arr;
  my_arr = c;
}

int intermediate_copy_assignment_const_ref_ok(const Arr& arr) {
  const auto& c = arr;
  return get_first_elem(c);
}

int intermediate_copy_assignment_const_value_bad(const Arr arr) {
  return get_first_elem(arr); // fix here is to remove const from param type
}

struct Wrapper {
  Arr my_field;
  const Arr& get_const_ref();

  void intermediate_const_ref_callee_ok() { get_first_elem(get_const_ref()); }

  void assignment_const_ref_ok() { my_field = get_const_ref(); }
};
class FVector {

  FVector(FVector const& rhs) {
    table_ = rhs.table_; // don't report on copy ctors
  }

  FVector& operator=(FVector const& rhs) {
    if (this != &rhs) {
      table_ = rhs.table_; // don't report on copy ctors
    }
    return *this;
  }

 protected:
  std::vector<int> table_;
};

void call_templated_func_specialized_int(const std::vector<int>& arg) {
  copy_in_header_bad(arg);
}

void call_templated_func_specialized_string(
    const std::vector<std::string>& arg) {
  copy_in_header_bad(arg);
}

class NonTrivialCopyClass {

 public:
  NonTrivialCopyClass& operator=(const NonTrivialCopyClass& rhs) {
    if (this == &rhs) {
      return *this;
    }
    return *this;
  }

 private:
  long my_int1_;
  long my_int2_;
};

class NonTrivialCopySmallClass {
 public:
  NonTrivialCopySmallClass& operator=(const NonTrivialCopySmallClass& rhs) {
    if (this == &rhs) {
      return *this;
    }
    return *this;
  }

 private:
  int my_int1_;
};

class FieldCopyClass {

 public:
  Arr my_arr1_;
  Arr my_arr2_;
  NonTrivialCopyClass nt_;
  NonTrivialCopySmallClass nt_small_;

  void set_field_ok(Arr arg) {
    my_arr1_ = my_arr2_; // rhs is a field, we cannot suggest move
  }

  void copy_assign_bad(NonTrivialCopyClass arg) { nt_ = arg; }

  void copy_assign_small_ok(NonTrivialCopySmallClass arg) { nt_small_ = arg; }

  void copy_assign_from_global_ok() {
    my_arr1_ = global; // rhs is a global, we cannot suggest move
  }

  FieldCopyClass* getPtr() { return this; }
  void copy_assign_from_this_pointed_ok() {
    auto t = getPtr();
    my_arr1_ = t->my_arr1_;
  }
};

void intermediate_copy_global_ok() {
  get_first_elem(global); // we cannot suggest moving global
}

class CopyConstructGlobalOk {
  Arr my_arr;

 public:
  CopyConstructGlobalOk() : my_arr(global) {}
};

void copy_from_global_bad(bool b) { auto x = b ? global : global; }

class CopyConstructFromRefOk {
  Arr my_arr;

 public:
  CopyConstructFromRefOk(Arr& arr) : my_arr(arr) {}
};

class CopyConstructFromRefBad1 {
  Arr my_arr;

 public:
  CopyConstructFromRefBad1(Arr arr) : my_arr(arr) {}
};

class CopyConstructFromRefBad2 {
  Arr my_arr;

 public:
  CopyConstructFromRefBad2(Arr&& arr) : my_arr(arr) {}
};

std::map<std::string, std::string> unreliable_source_ok(
    const std::map<std::string, std::string>& input) {
  auto modified = input; // the source of `modified` was `input` in the copy map
  for (const auto& elem : input) {
    modified[elem.first] = "abc";
  }
  return modified;
  // the source of `modified` was `__range` from the for loop iteration, thus it
  // did not correctly check `modified` is modified.
}

void copy_assignment_from_lvalue_ref_ok_intermediate_bad(std::string& str,
                                                         Arr arr) {
  std::string s;
  s = str; // no report here since we can't safely move str without affecting
           // callees
  int res = get_first_elem(arr); // we still wanna report here
}

void normal_copy_from_lvalue_ref_bad(std::string& str) {
  auto f = str; // still report here
}

void intermediate_copy_from_lvalue_ref_ok(Arr& arr) {
  get_first_elem(arr); // don't report since we can't safely move str without
                       // affecting callees
}

void intermediate_copy_from_pointer_ok(Arr* arr) { get_first_elem(*arr); }

std::string unknown_modification_twice_ok(std::string p) {
  std::string path = p;
  std::replace(path.begin(), path.end(), '\\', '/');
  return path;
}

void assert_false(
    const std::string& s) { // summary for this results in 0 disjuncts
  assert(false);
};

struct NonDisjJoin_ok {

  NonDisjJoin_ok(std::string myStr, int k) : NonDisjJoin_ok(myStr) {
    if (k > 0) {
      assert_false(myStr);
    } // here we were joining 0 disjuncts (with NonDisj.bottom) with Foo(myStr)
    // which resulted in ignoring the read of myStr in the conditional. Fixed
    // now.
  }

  NonDisjJoin_ok(std::string s) {}
};

struct NonDisjJoinLoop_ok_FP {

  NonDisjJoinLoop_ok_FP(std::string myStr, int k)
      : NonDisjJoinLoop_ok_FP(myStr) {
    for (int i = 0; i < k; i++) {
      assert_false(myStr);
    } // still doesn't work. TODO
  }

  NonDisjJoinLoop_ok_FP(std::string s) {}
};

struct Ptr {
  int* ptr;
  std::vector<int> vec;
};

Ptr* get_unknown_ptr_field();

Ptr global_ptr;
class UnownedTest {
  Arr field;
  Ptr ptr_field;
  std::vector<int> vec_field;
  int* ptr;

  void copy_assignment_points_to_global_ok() {
    auto& ref = global;
    field = ref; // we can't suggest moving here
  }

  void copy_assignment_copy_of_global_bad() {
    auto ref = global;
    field = ref; // moving is ok here
  }

  Arr* get_global_ptr() { return &global; }

  void copy_assignment_via_callee_points_to_global_ok() {
    auto& ref = get_global_ptr()->vec;
    vec_field = ref; // we can't suggest moving here
  }

  void copy_assignment_points_to_ref_param_ok(Arr& param) {
    auto& ref = param;
    field = ref; // we can't suggest moving here
  }

  void intermediate_copy_points_to_global_ok() {
    auto& ref = global;
    get_first_elem(ref);
  }

  void intermediate_copy_copy_of_global_bad() {
    auto ref = global;
    get_first_elem(ref); // ok to move here
  }

  void intermediate_copy_via_callee_points_to_global_ok() {
    auto& ref = get_global_ptr()->vec;
    modify_arg(ref); // we can't suggest moving here
  }

  void intermediate_copy_points_to_ref_param_ok(Arr& param) {
    auto& ref = param;
    get_first_elem(ref);
  }

  void copy_assignment_copy_ptr_bad() {
    Ptr c{};
    c.ptr = global_ptr.ptr;
    ptr_field = c; // ok to move here
  }

  void copy_assign_from_unknown_ok() { ptr_field = *get_unknown_ptr_field(); }
};

struct ArrWrap {
  Arr arr;
};

void captured_by_ref_ok() {
  Arr x;
  auto _ = [&]() {
    ArrWrap y;
    y.arr = x;
    return y;
  };
  x.arr[0] = 42;
}
