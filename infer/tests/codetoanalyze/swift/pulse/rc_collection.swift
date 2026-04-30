// Coverage for retain cycles that close through a collection (here, an Array).
// All existing pulse cycle tests close the loop through a single stored property
// — this exercises the case where one side of the cycle is mediated by the
// implicit storage of `Array<Element>`.

class Parent {
    var children: [Child] = []
}

class Child {
    var parent: Parent?
}

class WeakParent {
    var children: [WeakChild] = []
}

class WeakChild {
    weak var parent: WeakParent?
}

// Strong: Parent.children -> [Child]. Strong-back: Child.parent -> Parent.
// This is a real retain cycle, but Pulse currently does not chase cycles
// through `Array` storage and reports nothing — false negative, hence `_FN`.
// The day Pulse models cycles through `Array<Element>` (and other Swift
// collections), this should start reporting `RETAIN_CYCLE` and the suffix
// can be dropped.
func test_collection_cycle_bad_FN() {
    let p = Parent()
    let c = Child()
    c.parent = p
    p.children.append(c)
}

// Strong: WeakParent.children -> [WeakChild]. Weak-back: WeakChild.parent -> WeakParent.
// No cycle. Mirror of the `weak`-based store on a single property — but routed
// through the array. Should NOT fire.
func test_collection_weak_no_cycle_good() {
    let p = WeakParent()
    let c = WeakChild()
    c.parent = p
    p.children.append(c)
}
