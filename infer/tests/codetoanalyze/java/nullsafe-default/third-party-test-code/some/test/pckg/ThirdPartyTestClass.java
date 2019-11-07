/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package some.test.pckg;

/**
 * A test third party class. We specify its annotations outside of this class, in a third-party
 * repository.
 */
public class ThirdPartyTestClass {

  // Return values.

  // No information in 3rd party repo
  public String returnUnspecified() {
    return "";
  }

  // 3rd party repo whitelists this function as returning non-nullable
  public String returnSpecifiedAsNonnull() {
    return "";
  }

  // 3rd party repo whitelists this function as returning nullable
  public String returnSpecifiedAsNullable() {
    return "";
  }

  // Params.

  // No information about this function in 3rd party repo
  public void paramUnspecified(String param) {}

  public void secondParamSpecifiedAsNonnull(
      String specifiedAsNullable, String specifiedAsNonnull) {}
}
