func test_getelementptr_segfault(a: Int, b:Int) -> [String: String] {
    return [
        "a": String(a),
        "b": String(b)
    ]
}
