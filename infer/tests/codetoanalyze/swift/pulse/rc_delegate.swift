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


final class Cell2 {

    var contentView: ContentViewType?

    func configure() {
        let newContentView = Utils.contentViewForVariant()
        contentView = newContentView
        contentView?.delegate = self
        contentView?.configure()
    }

    func test() {
        contentView?.delegate = self
    }
}

func test_retain_cycle_specialisation2_bad() {
    let cell = Cell2()
    cell.contentView = DummyContentView()
    cell.test()
}
