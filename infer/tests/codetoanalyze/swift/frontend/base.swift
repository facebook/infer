
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

func test5_fp() {
    let person_age = test4()
    let age : Int = 30
    assert(person_age == age)
}

class Person {
    let age: Int
    var spouse: Person?
    init(age: Int) {
        self.age = age
    }
}

func test6() -> Person {
    let john = Person(age: 30)
    return john
}
