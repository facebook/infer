// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// In this file, the tainted value is never in the class, nor in a trait,
// so parent takes precedence

// Depends on definitions found in method_resolution_trait1.hack

class Bad30 extends BadP {
}

class Ok30 extends OkP {
}

function basicFlowBad30(): void {
  $c = new Bad30();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk30(): void {
  $c = new Ok30();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}
