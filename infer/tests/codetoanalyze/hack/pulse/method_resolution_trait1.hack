// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// In this file, the tainted value is always in the class, so it must
// have priority over any trait or parent class.

class Bad10 {
  public function runSource(): int {
    return \Level1\taintSource();
  }
}

class Ok10 {
  public function runSource(): int {
    return 42;
  }
}

function basicFlowBad10(): void {
  $c = new Bad10();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk10(): void {
  $c = new Ok10();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}

trait OkT {
  public function runSource(): int {
    return 42;
  }
}

trait BadT {
  public function runSource(): int {
    return \Level1\taintSource();
  }
}

class Bad11 {
  use OkT;
  public function runSource(): int {
    return \Level1\taintSource();
  }
}

class Ok11 {
  use BadT;
  public function runSource(): int {
    return 42;
  }
}

function basicFlowBad11(): void {
  $c = new Bad11();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk11(): void {
  $c = new Ok11();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}

class OkP {
  public function runSource(): int {
    return 42;
  }
}

class BadP {
  public function runSource(): int {
    return \Level1\taintSource();
  }
}

class Bad12 extends OkP {
  public function runSource(): int {
    return \Level1\taintSource();
  }
}

class Ok12 extends BadP {
  public function runSource(): int {
    return 42;
  }
}

function basicFlowBad12(): void {
  $c = new Bad12();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk12(): void {
  $c = new Ok12();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}

class Bad13 extends OkP {
  use OkT;
  public function runSource(): int {
    return \Level1\taintSource();
  }
}

class Ok13 extends BadP {
  use BadT;
  public function runSource(): int {
    return 42;
  }
}

function basicFlowBad13(): void {
  $c = new Bad13();
  $tainted = $c->runSource();
  \Level1\taintSink($tainted);
}

function basicFlowOk13(): void {
  $c = new Ok13();
  $untainted = $c->runSource();
  \Level1\taintSink($untainted);
}
