// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class IntraFileFlow {
  public static function explicitSinkMethodDirectBad(SensitiveClass $sc): void {
    // This is a base case: sensitive data flows directly into the taint sink
    UnknownClass::explicitSinkAllArgs($sc);
  }

  public static function explicitSinkMethodDirectOk(int $i): void {
    // Untainted data flowing into taint sink is OK
    UnknownClass::explicitSinkAllArgs($i);
  }

  public static function explicitSinkMethodDerivedBad(
    SensitiveClass $sc,
  ): void {
    // Here we have data which is derived from taint source and flows directly into a taint sink
    $derived = $sc->getDerived();
    UnknownClass::explicitSinkAllArgs($derived);
  }

  public function FN_explicitSinkMethodDerivedDynamicBad(
    SensitiveClass $sc,
  ): void {
    $derived = $sc->getDerived();
    // Here we have derived data flowing into a taint sink across the function boundary. The
    // analysis doesn't understand that the target of the call is `callExplicitSinkAllArgs` below
    // because the call goes thru HackMixed.
    $this->callExplicitSinkAllArgs($derived);
  }

  public static function FN_explicitSinkClassDirectBad(
    SensitiveClass $sc,
  ): void {
    // Bugs around procname / classname matching
    Logger::someLogMethod($sc);
  }

  // Helpers

  private function callExplicitSinkAllArgs(int $data): void {
    UnknownClass::explicitSinkAllArgs($data);
  }
}
