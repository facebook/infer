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
    var weight: Int
    init(age: Int, weight: Int) {
        self.age = age
        self.weight = weight
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


func setEmployeeWeight( _ weight: Int) {
    var john = Employee(age: 30, weight: 60)
    john.weight = weight
}
