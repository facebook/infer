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
struct Quality: Equatable {
    let value: Int
    let stringValue: String
    init?(value: Int) {
        guard value > 0 && value <= 100 else { return nil }
        self.value = value
        self.stringValue = "q\(value)"
    }
}

@objc final public class Param: NSObject {
    let size: TestSize?
    let quality: Quality?
    @objc public let stringValue: String
    init(size: TestSize?, quality: Quality?) {
        self.size = size
        self.quality = quality
        var result: [String] = []
        if let size = size { result.append(size.stringValue) }
        if let quality = quality { result.append(quality.stringValue) }
        stringValue = result.sorted(by: { $0 < $1 }).joined(separator: "_")
    }
 }
