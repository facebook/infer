
func returnOne() -> Int {
    return 1
}

func test1(_ n: Int, _ n2 : Int) -> Int {
    return n
}

func test2() -> Int {
    return returnOne()
}

func test3(_ n : Int, _ n2 : Int) -> Int {
    return test1(n, n2)
}

func createPerson(age: Int, height : Int) -> (age: Int, height: Int) {
    return (age, height)
}

func test4() -> Int {
    let person = createPerson(age: 30, height: 180)
    return person.age
}

class Person {
    let age: Int
    var spouse: Person
    init(age: Int) {
        self.age = age
        self.spouse = Person(age: 0)
    }
}

func test6() -> Person {
    return Person(age: 30)
}

func set_spouses(_ john: Person, _ jane: Person) {
    john.spouse = jane
    //jane.spouse = john
}

func test_retain_cycle() {
    let john = Person(age: 30)
    let jane = Person(age: 35)
    set_spouses(john, jane)
}
