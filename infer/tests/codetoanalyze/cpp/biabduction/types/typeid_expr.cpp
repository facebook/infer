/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <typeinfo>

class Person {
 public:
  virtual ~Person() {}
};

class Employee : public Person {};

int person_typeid() {
  Person person;
  int t = 3;
  if (typeid(t) == typeid(person))
    return 1;
  else
    return 1 / 0;
}

int person_typeid_name() {
  Person person;
  int t = 3;
  const char* t_type_info = typeid(t).name();
  const char* person_type_info = typeid(person).name();
  if (t_type_info == person_type_info)
    return 0;
  else
    return 1 / 0;
}

int employee_typeid() {
  Employee employee;
  Person* ptr = &employee;
  if (typeid(employee) == typeid(*ptr))
    return 1 / 0;
  else
    return 0;
}

int person_ptr_typeid(Person* ptr) {
  Person person;
  if (typeid(*ptr).name() == typeid(person).name())
    return 1 / 0;
  else
    return 0;
}

template <class Tgt>
const char* template_typeid(const Tgt& value) {
  Tgt result = Tgt(value);
  return typeid(Tgt).name();
}

int template_type_id_person() {
  Person person;
  if (template_typeid<Person>(person) == typeid(person).name())
    return 1;
  else
    return 1 / 0;
}
