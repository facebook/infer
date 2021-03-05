/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.util.*;

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

    return new_p;
  }

  public void testInstanceOfObjStudentOk() {
    Person p = new Student();
    if (p instanceof Student) {
      Person x = updatePerson(p);
      x.toString();
    }
  }

  public void testInstanceOfObjProfessorBad() {
    Person p = new Professor();
    Person new_p = updatePerson(p);
    new_p.toString();
  }

  public void testInstanceOfObjProfessorOk() {
    Person p = new Professor();
    if (p instanceof Student) {
      Person new_p = updatePerson(p);
      new_p.toString();
    }
  }

  public void testInstanceOfNullOk() {
    Person p = new Professor();
    // p is null after this call
    p = updatePerson(p);
    // null is not an instance of any class
    if (p instanceof Professor) {
      p.toString();
    }
  }

  public void checkInstanceArray(Object array) {
    Object o = null;
    if (array instanceof int[]) {
      o = new Object();
    }

    if (array instanceof boolean[]) {
      o.toString();
    }
  }

  public void testInstanceOfIntArrayOk() {
    int arr[] = new int[10];
    checkInstanceArray(arr);
  }

  public void testInstanceOfBooleanArrayBad() {
    boolean arr[] = new boolean[10];
    checkInstanceArray(arr);
  }

  public List<Integer> createsEmptyList() {
    return new ArrayList<Integer>();
  }

  public void testInstanceOfArrayListOk() {
    List<Integer> elems = createsEmptyList();
    Person p = null;
    if (elems instanceof ArrayList) {
      p = new Student();
    }
    p.toString();
  }

  public String getClassByName(Object o) {
    return o.getClass().getName();
  }

  /*
   * This example triggers a call to instanceof with a Var expression instead of a literal type.
   * This only happens in some peculiar cases. For instance, if we inline the getClassByName call
   * we don't have this problem. For now, we do not handle this example properly but we hope to
   * investigate soon.
   */
  public void requiresFurtherInvestigationOk(List<Person> people) {
    people.stream().filter(p -> getClassByName(p).equals("Student") && p instanceof Student);
  }
}
