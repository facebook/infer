class Student {
    let age: Int
    init(age: Int) {
        self.age = age
    }
}

func create_student() -> Int {
    let john = Student(age: 30)
    return john.age
}

func test_student_allocation_bad() {
    assert(create_student() == 35)
}

func test_student_allocation_good() {
    assert(create_student() == 30)
}
