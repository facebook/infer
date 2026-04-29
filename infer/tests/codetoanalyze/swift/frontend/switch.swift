// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

func switch_on_int(_ x: Int) -> Int {
    switch x {
    case 0: return 10
    case 1: return 20
    case 2: return 30
    default: return 40
    }
}

func switch_on_bool(_ b: Bool) -> Int {
    switch b {
    case true: return 1
    case false: return 0
    }
}

enum Color {
    case red
    case green
    case blue
}

func switch_on_enum(_ c: Color) -> Int {
    switch c {
    case .red: return 1
    case .green: return 2
    case .blue: return 3
    }
}
