/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
public class InstanceOfExample {
  abstract class Person {}

  class Faculty extends Person {}

  class Professor extends Faculty {}

  class Student extends Person {}

  public void testInstanceOfObjFacultyOk() {
    Person p = new Professor();
    Person p2 = null;
    if (p instanceof Faculty) {
      p2 = new Student();
    }
    p2.toString();
  }

  public Person updatePerson(Person p) {
    Person new_p = null;
    if (p instanceof Student) {
      new_p = p;
    }

    if (p instanceof Professor) {
      new_p = null;
    }

    return new_p;
  }

  public void FP_testInstanceOfObjProfessorOk() {
    Person p = new Professor();
    if (p instanceof Student) {
      String x = updatePerson(p).toString();
    }
  }

  public void FP_testInstanceOfObjStudentOk() {
    Person p = new Student();
    Person new_p = updatePerson(p);
    new_p.toString();
  }

  public Object checkInstanceArray(Object array) {
    Object o = null;
    if (array instanceof boolean[]) {
      o = new Object();
    }

    if (array instanceof int[]) {
      o.toString();
    }

    return o;
  }

  public void FN_testInstanceOfArrayIntBad() {
    int arr[] = new int[10];
    checkInstanceArray(arr);
  }
}
