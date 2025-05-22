
func returnOne() -> Int {
    return 1
}

func test1(_ n: Int) -> Int {
    return n
}

func test2() -> Int {
    return returnOne()
}

func test3(_ n : Int) -> Int {
    return test1(n)
}
