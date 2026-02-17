import UIKit
import Foundation

protocol ContentViewType {
    func configure()
    func value() -> Int
    var delegate: AnyObject? { get set }
}

struct Utils {

    static func contentViewClassForVariant() -> ContentViewType.Type {
        return DummyContentView.self
    }

    static func contentViewForVariant() -> ContentViewType {
        return DummyContentView()
    }
}

class DummyContentView: ContentViewType {
    var delegate: AnyObject?
    func configure() {
    }
    func value() -> Int  {
        return 15
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
        let contentViewType = Utils.contentViewClassForVariant()
        if let unwrappedContentView = contentView {
            if type(of: unwrappedContentView) != contentViewType {
                contentView = nil
            }
        }
        if contentView == nil {
            let newContentView = Utils.contentViewForVariant()
            contentView = newContentView
            contentView?.delegate = self
        }
        contentView?.configure()
    }

    func test() {
        contentView?.delegate = self
    }

    func test2() -> Int? {
        return contentView?.value()
    }
}

func test_retain_cycle_specialisation2_bad() {
    let cell = Cell2()
    cell.contentView = DummyContentView()
    cell.test()
}

func test_value() -> Int? {
    let cell = Cell2()
    cell.contentView = Utils.contentViewForVariant()
    return cell.test2()
}

func test_value_nil() -> Int? {
    let cell = Cell2()
    cell.contentView = nil
    return cell.contentView?.value()
}

func test_value_nil2_good() -> Int {
    let cell = Cell2()
    cell.contentView = Utils.contentViewForVariant()
    return cell.contentView?.value() ?? 0
}

func test_value_nil3_good(_ contentView: ContentViewType?) -> Int {
    return contentView?.value() ?? 0
}
