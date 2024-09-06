// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// this wasn't't originally intended to produce signal - just for looking at
// debug info to check initialisation of constants is
// happening correctly
// However it turns out to demonstrate that we're not dealing with constants in
// interfaces properly yet, so there is an FP in there at the moment
namespace ConstantInit;

interface I {
  const int Ic = 3;
}

class C implements I {
  const int Cc = 1;
}

trait T {
  const int Tc = 4;
}

class D extends C {
  use T;
  const int Dc = 2;
}

class Tester {
  public static async function mainFP(): Awaitable<void> {
    $d = D::Dc;
    // at this point we should have called, and marked as
    // called, C$static._86init
    // So the subsequent access to C::Cc shouldn't call constinit first
    $c = C::Cc;
    // next bit shows we're not dealing with constants in interfaces properly yet
    $i = C::Ic;
    if ($i === 3) {
      return; // should always happen
    }
    $_ = async {
      return; // UAA error
    };
  }
}
