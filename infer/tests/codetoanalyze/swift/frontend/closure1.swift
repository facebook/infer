class RetainCycleExample {
    var id = 10
    // The closure property
    var closure: (() -> Void) = {}
}


func createObject() -> RetainCycleExample {
    let obj = RetainCycleExample()
    obj.closure = { obj.id = 20 }
    return obj
}

func test() -> Int {
    let obj = createObject()
    obj.closure()
    return obj.id
}


func example(_ closure: @escaping (Int) -> Void) {
    closure(20)
}

func testWithClosure() -> Int {
   let obj = RetainCycleExample()
    example { n in
        obj.id = n
    }
    return obj.id
}

func example2(using closure: @escaping (RetainCycleExample) -> Void) -> Int {
    let obj = RetainCycleExample()
    closure(obj)
    return obj.id
}

func testWithClosure2() -> Int {
    return example2 { obj in
        obj.id = 20
    }
}
