// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ControlFlow;


function typeCheckDoesntConfuseTheAnalysis_maintainsTaint_Bad(mixed $arg1, SensitiveClass $sc): void {
  if ($arg1 is Foo) {
    \Level1\taintSink($sc);
  }
}

class C {
  public mixed $data;
}

function nullsafeLog(?C $arg): void {
  \Level1\taintSink($arg?->data);
}

function nullsafeAccessTaintedBad(SensitiveClass $sc): void {
  $c = new C();
  $c->data = $sc;
  nullsafeLog($c);
}

function nullsafeAccessNullOk(): void {
  nullsafeLog(null);
}

function logWhenNonnull(?mixed $arg): void {
  if ($arg is nonnull) {
    \Level1\taintSink($arg->data);
  }
}

function loggingSensitiveNonnullBad(SensitiveClass $sc): void {
  logWhenNonnull($sc);
}

function loggingSensitiveNonnullCheckedBad(?SensitiveClass $sc): void {
  if ($sc is nonnull) {
    logWhenNonnull($sc);
  }
}

function loggingSensitiveWhenNullOk(?SensitiveClass $sc): void {
  if ($sc is null) {
    logWhenNonnull($sc);
  }
}
