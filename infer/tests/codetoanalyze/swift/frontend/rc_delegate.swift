import UIKit
import Foundation

protocol ContentViewType {
    func configure()
    var delegate: AnyObject? { get set }
}

struct Utils {

    static func contentViewForVariant() -> ContentViewType {
        return DummyContentView()
    }
}

class DummyContentView: ContentViewType {
    var delegate: AnyObject?
    func configure() {
    }
}

final class CreationCell {

    var contentView: ContentViewType?

    func configure() {
        contentView = Utils.contentViewForVariant()
        contentView?.delegate = self
    }
}

func test_retain_cycle_bad_fn() {
    let cell = CreationCell()
    cell.configure()
}

final class Cell {

    var contentView: ContentViewType = DummyContentView()

    func configure() {
        contentView = Utils.contentViewForVariant()
        contentView.delegate = self
    }
}
