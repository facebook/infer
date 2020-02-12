/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <string>

class smart_ptr {
 public:
  class my_class {
   public:
    my_class(int i) {
      int a[5];
      a[i] = 0;
    }

    my_class(int i, int j) {
      int a[10];
      a[i + j] = 0;
    }

    my_class(const std::string& str, int i) {
      int a[5];
      a[i] = 0;
    }

    int i;

    void set_i(int n) { i = n; }

    void array_access() {
      int a[5];
      a[i] = 0;
    }
  };

  void use_shared_ptr1_Good() {
    int i = 3;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(i);
  }

  void use_shared_ptr1_Bad() {
    int i = 8;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(i);
  }

  void use_shared_ptr2_Good() {
    int i = 3;
    int j = 5;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(i, j);
  }

  void use_shared_ptr2_Bad() {
    int i = 8;
    int j = 8;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(i, j);
  }

  void shared_ptr_with_std_string_Good() {
    std::string str = "abc";
    int i = 3;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(str, i);
  }

  void shared_ptr_with_std_string_Bad() {
    std::string str = "abc";
    int i = 8;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(str, i);
  }

  void shared_ptr_with_const_int_Good() {
    const int i = 3;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(i);
  }

  void shared_ptr_with_const_int_Bad() {
    const int i = 8;
    std::shared_ptr<my_class> p = std::make_shared<my_class>(i);
  }

  void call_method_Good() {
    std::shared_ptr<my_class> p = std::make_shared<my_class>(0);
    p->set_i(3);
    p->array_access();
  }

  void call_method_Bad() {
    std::shared_ptr<my_class> p = std::make_shared<my_class>(0);
    p->set_i(8);
    p->array_access();
  }
};
