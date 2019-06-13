/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import junit.framework.TestCase;

public class JunitExample extends TestCase {

  private Object mField;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    mField = new Object();
  }

  public void testSomething() {
    mField.toString();
  }
}
