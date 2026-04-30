// Coverage for `unowned` references — companion to the `weak`-based cycle tests
// in `student.swift`. Pulse should treat `unowned` as a cycle breaker (same as
// `weak`), so neither of these patterns should report a RETAIN_CYCLE.

class Owner {
    let id: Int
    var card: Card?
    init(id: Int) {
        self.id = id
    }
}

class Card {
    let number: Int
    unowned let owner: Owner
    init(number: Int, owner: Owner) {
        self.number = number
        self.owner = owner
    }
}

// Strong: Owner.card -> Card. Unowned-back: Card.owner -> Owner.
// No retain cycle should be reported, but Pulse currently treats `unowned`
// stored properties as strong references and falsely fires a RETAIN_CYCLE
// here — hence the `_FP` suffix. The closure-capture form below
// (`[unowned self]`) is modelled correctly.
func test_unowned_property_no_cycle_good_FP() {
    let owner = Owner(id: 1)
    let card = Card(number: 1234, owner: owner)
    owner.card = card
}

class UnownedClosureExample {
    var id = 10
    var closure: (() -> Void)?

    // Mirror of `RetainCycleExample.setupClosureOk` in `student.swift` but with
    // `[unowned self]` instead of `[weak self]`. No retain cycle should fire.
    func setupClosureUnownedOk() {
        closure = { [unowned self] in
            self.id = 20
        }
    }
}
