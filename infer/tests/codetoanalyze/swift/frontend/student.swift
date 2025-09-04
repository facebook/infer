class Student {
    let age: Int
    var weight: Int
    init(age: Int, weight: Int) {
        self.age = age
        self.weight = weight
    }
}

func create_student() -> Int {
    let john = Student(age: 30, weight: 60)
    return john.age
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
