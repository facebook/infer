import Foundation
import CoreGraphics

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


@objc public enum SizeMode: Int {
    case sBox
    case pBox
    case invalid
}
struct TestSize: Equatable {
    let width: Int
    let height: Int
    let mode: SizeMode
    let stringValue: String
    init?(size: CGSize, mode: SizeMode) {
        self.width = Int(size.width)
        self.height = Int(size.height)
        self.mode = mode
        self.stringValue = "\(width)x\(height)"
    }
}

@objc final public class CTPParam: NSObject { }
