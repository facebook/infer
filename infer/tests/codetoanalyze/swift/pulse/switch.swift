// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// switch on Int: lowered as if-then-else by LLVM, translated correctly by the
// frontend, so Pulse can reason about the result precisely.

func choose_int(_ x: Int) -> Int {
    switch x {
    case 0: return 10
    case 1: return 20
    case 2: return 30
    default: return 40
    }
}

func test_switch_int_bad() {
    let r = choose_int(0)
    assert(r == 99) // true positive: r is 10, not 99
}

func test_switch_int_good() {
    let r = choose_int(0)
    assert(r == 10) // true negative: r is 10
}

// switch on Bool: hits the multi-entry Llair.Switch case in
// Llair2Textual.ml and is currently translated as Unreachable, so Pulse
// has no precise summary for the callee.

func choose_bool(_ x: Bool) -> Int {
    switch x {
    case true: return 10
    case false: return 20
    }
}

func test_switch_bool_good_FP() {
    let r = choose_bool(true)
    // false positive: r is 10 so the assertion holds, but Pulse has no
    // summary for choose_bool (translated as Unreachable) and fires.
    assert(r == 10)
}

func test_switch_bool_bad() {
    let r = choose_bool(true)
    assert(r == 99) // true positive: r is 10, not 99
}

// switch on enum: same situation as Bool — multi-entry switch lowered as
// Unreachable today.

enum Direction {
    case north
    case south
    case east
    case west
}

func choose_enum(_ d: Direction) -> Int {
    switch d {
    case .north: return 10
    case .south: return 20
    case .east: return 30
    case .west: return 40
    }
}

func test_switch_enum_good_FP() {
    let r = choose_enum(.north)
    // false positive: r is 10 so the assertion holds, but Pulse has no
    // summary for choose_enum (translated as Unreachable) and fires.
    assert(r == 10)
}

func test_switch_enum_bad() {
    let r = choose_enum(.north)
    assert(r == 99) // true positive: r is 10, not 99
}
