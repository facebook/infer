class Human {
    let age: Int
    var spouse: Human?
    init(age: Int) {
        self.age = age
    }
}

func set_spouses(_ john: Human, _ jane: Human) {
    john.spouse = jane
    jane.spouse = john
}

func test_retain_cycle_basic_bad() {
    let john = Human(age: 30)
    let jane = Human(age: 35)
    set_spouses(john, jane)
}
