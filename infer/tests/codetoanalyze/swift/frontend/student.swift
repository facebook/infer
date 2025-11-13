class Student {
    let age: Int
    var weight: Int
    init(age: Int, weight: Int) {
        self.age = age
        self.weight = weight
    }
}

func create_student() -> Int {
    return Student(age: 30, weight: 60).age
}

func test_student_allocation_bad() {
    assert(create_student() == 35)
}

func test_student_allocation_good() {
    assert(create_student() == 30)
}


func setWeight( _ weight: Int) {
    let john = Student(age: 30, weight: 60)
    john.weight = weight
}

struct Employee {
    let age: Int
    private var _weight: Int
    var weight: Int {
        get { _weight }
        set { _weight = newValue }
    }
    init(age: Int, weight: Int) {
        self.age = age
        self._weight = weight
    }
}

func create_employee() -> Int {
    return Employee(age: 30, weight: 60).age
}

func test_employee_allocation_bad() {
    assert(create_employee() == 35)
}

func test_employee_allocation_good() {
    assert(create_employee() == 30)
}


func setEmployeeWeight( _ weight: Int) -> Int {
    var john = Employee(age: 30, weight: 60)
    john.weight = weight
    return john.weight
}

func test_employee_getter_setter_bad() {
    assert(setEmployeeWeight(70) == 75)
}

func test_employee_getter_setter_good() {
    assert(setEmployeeWeight(70) == 70)
}

class Person {
    let age: Int
    var spouse: Person
    init(age: Int) {
        self.age = age
        self.spouse = Person(age: 0)
    }
}

func test_retain_cycle_bad(_ john: Person, _ jane: Person) {
    john.spouse = jane
    jane.spouse = john
}

func createPerson() -> Person {
    fatalError("Fail here")
}


func test_retain_cycle_person1_bad(_ jane: Person) {
    let john = createPerson()
    john.spouse = jane
    jane.spouse = john
}



class Individual {
    let age: Int
    weak var spouse: Individual?
    init(age: Int) {
        self.age = age
    }
}

func set_spouses(_ john: Individual, _ jane: Individual) {
    john.spouse = jane
    jane.spouse = john
}

func test_retain_cycle1_bad() {
    let john = Individual(age: 30)
    let jane = Individual(age: 35)
    set_spouses(john, jane)
}

func test_optional(_ age : Int?) -> Int {
    if let actualAge = age {
        return actualAge
    }
    else {
        return 0
    }
}

func test_optional_good_fp() {
    assert(test_optional(30) == 30)
}

func test_optional_bad() {
    assert(test_optional(30) == 35)
}
