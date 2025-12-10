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


func example(using closure: @escaping (RetainCycleExample) -> Void) -> Int {
    let obj = RetainCycleExample()
    closure(obj)
    return obj.id
}

func testWithClosure() -> Int {
    return example { obj in
        obj.id = 20
    }
}
