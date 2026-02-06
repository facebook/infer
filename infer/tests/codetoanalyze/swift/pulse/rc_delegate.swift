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

    var contentView: ContentViewType = DummyContentView()

    func configure() {
        contentView.delegate = self
    }
}

func test_retain_cycle_bad() {
    let cell = CreationCell()
    cell.contentView.delegate = cell
}

func test_retain_cycle_specialisation_bad() {
    let cell = CreationCell()
    cell.configure()
}

final class Cell {

    var contentView: ContentViewType = DummyContentView()

    func configure_bad() {
        contentView = Utils.contentViewForVariant()
        contentView.delegate = self
    }
}
