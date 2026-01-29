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

    @objc public convenience init(size: CGSize, mode: SizeMode) {
        let newSize = TestSize(size: size, mode: mode)
        self.init(size: newSize, quality: nil)
    }
    @objc public convenience init(size: CGSize, mode: SizeMode, quality: Int) { //retain cycle fp
        let newSize = TestSize(size: size, mode: mode)
        let quality = Quality(value: quality)
        self.init(size: newSize, quality: quality)
    }
}

struct ImageURL {
    let url: String
}

func return_string(url: String) -> String {
    return ""
}

func my_test() -> Set<String> {
    let ascendingSizeImageURLs = [ImageURL(url: "https://example.com/image1.jpg")]
    var urlIdentifiersToTrack = Set<String>()
    ascendingSizeImageURLs.forEach { imageUrl in
        let urlIdentifier = return_string(url: imageUrl.url)
        urlIdentifiersToTrack.insert(urlIdentifier)
    }
    return urlIdentifiersToTrack
}

import UIKit
// Define the spacing constant
let kIGDSSpacingStandard: CGFloat = 8.0
// Function to return the spacing value
func getSpacing() -> CGFloat {
    return kIGDSSpacingStandard
}
// Your custom cell class
final class BCNTypeaheadSuggestionsCell: UICollectionViewCell {
    // Static property using getSpacing()
    private static let contentInsets = UIEdgeInsets(
        top: getSpacing(),
        left: getSpacing(),
        bottom: getSpacing(),
        right: getSpacing()
    )
}

public struct TextRendererConfig {
  public struct Padding: Sendable  {
    public let top: Float
    public let bottom: Float
    public let sides: Float

    public init(
      top: Float = 5,
      bottom: Float = 5,
      sides: Float = 4
    ) {
      self.top = top
      self.bottom = bottom
      self.sides = sides
    }

    public static let `default` = Padding()
  }
}

struct Padding  {
    let top: Float
    let bottom: Float
    let sides: Float
}

func test() -> Float {
    let padding = Padding(top: 1.0, bottom: 2.0, sides: 3.0)
    return padding.top
}
