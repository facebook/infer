import UIKit
import Foundation

protocol ContentViewType {
    func configure()
    var delegate: AnyObject? { get set }
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
