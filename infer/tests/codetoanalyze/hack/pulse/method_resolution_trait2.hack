// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// In this file, the tainted value is never in the class, so leftmost trait must have
// precedence, and trait over parent class

// Depends on definitions found in method_resolution_trait1.hack
class Bad20 {
  use BadT;
}

class Ok20 {
  use OkT;
}

function basicFlowBad20(): void {
  $c = new Bad20();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk20(): void {
  $c = new Ok20();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}

class Bad21 extends BadP {}
class Ok21 extends OkP {}

function basicFlowBad21(): void {
  $c = new Bad21();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk21(): void {
  $c = new Ok21();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}

class Bad22 extends OkP {
  use BadT;
}

class Ok22 extends BadP {
  use OkT;
}

function basicFlowBad22(): void {
  $c = new Bad22();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk22(): void {
  $c = new Ok22();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}
