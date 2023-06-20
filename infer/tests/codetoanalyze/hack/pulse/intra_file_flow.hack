// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace IntraFile;

class IntraFileFlow {
  public static function FN_explicitSinkMethodDirectOnHackMixedSinkBad(SensitiveClass $sc): void {
    HackMixed::explicitSinkAllArgs($sc);
  }

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

  public function explicitSinkMethodDerivedDynamicBad(
    SensitiveClass $sc,
  ): void {
    $derived = $sc->getDerived();
    // Here we have derived data flowing into a taint sink across the function boundary. The
    // analysis has to understand that the target of the call is `callExplicitSinkAllArgs` below
    // using the static type of $this.
    $this->callExplicitSinkAllArgs($derived);
  }

  public static function explicitSinkClassDirectBad(
    SensitiveClass $sc,
  ): void {
    Logger::someLogMethod($sc);
  }

  // Helpers

  private function callExplicitSinkAllArgs(int $data): void {
    UnknownClass::explicitSinkAllArgs($data);
  }
}

class UnknownClass {
  public static function explicitSinkAllArgs(mixed $_): void {}
}

class Logger {
  public static function someLogMethod(mixed $_): void {}
}
