import UIKit
import Foundation

// MARK: - Model

public enum ProtocolObjCVariant: String {
    case rollDice = "ROLL_DICE"
    case slidingPrompts = "SLIDING_PROMPTS"
}

public final class ProtocolObjCModel: NSObject {
    public let title: String
    public let subtitle: String
    public let cta: String
    public let variant: ProtocolObjCVariant
    public let prompts: [String]
    public let uuid: String

    public init(
        title: String,
        subtitle: String,
        cta: String,
        variant: ProtocolObjCVariant,
        prompts: [String]
    ) {
        self.title = title
        self.subtitle = subtitle
        self.cta = cta
        self.variant = variant
        self.prompts = prompts
        self.uuid = UUID().uuidString
    }
}

// MARK: - ViewModel

public final class ProtocolObjCViewModel: NSObject {
    public let model: ProtocolObjCModel
    public let minimumHeight: CGFloat

    public init(model: ProtocolObjCModel, minimumHeight: CGFloat) {
        self.model = model
        self.minimumHeight = minimumHeight
    }
}

// MARK: - ContentView Protocol

protocol ProtocolObjCContentViewType: UIView {
    func configure(with viewModel: ProtocolObjCViewModel)
    var delegate: AnyObject? { get set }
}

// MARK: - ContentView Factory

struct ProtocolObjCViewUtils {
    static func contentViewClassForVariant(_ variant: ProtocolObjCVariant) -> UIView.Type {
        // For demonstration, always return ProtocolObjCDummyContentView
        return UIView.self
    }
    @MainActor
    static func contentViewForVariant(_ variant: ProtocolObjCVariant) -> ProtocolObjCContentViewType {
        // For demonstration, always return ProtocolObjCDummyContentView
        return ProtocolObjCDummyContentView()
    }
}

// Dummy implementation for demonstration
class ProtocolObjCDummyContentView: UIView, ProtocolObjCContentViewType {
    var delegate: AnyObject?
    func configure(with viewModel: ProtocolObjCViewModel) {
        // Configure dummy content
        backgroundColor = .lightGray
    }
}

// MARK: - Cell

final class ProtocolObjCCell: UICollectionViewCell {
    var viewModel: ProtocolObjCViewModel?
    private lazy var cardView: UIView = {
        let v = UIView()
        v.backgroundColor = .white
        v.layer.cornerRadius = 8
        v.translatesAutoresizingMaskIntoConstraints = false
        return v
    }()
    private lazy var titleLabel: UILabel = {
        let l = UILabel()
        l.font = UIFont.boldSystemFont(ofSize: 18)
        l.translatesAutoresizingMaskIntoConstraints = false
        return l
    }()
    private lazy var subtitleLabel: UILabel = {
        let l = UILabel()
        l.font = UIFont.systemFont(ofSize: 14)
        l.textColor = .darkGray
        l.translatesAutoresizingMaskIntoConstraints = false
        return l
    }()
    var nuxContentView: ProtocolObjCContentViewType?

    override init(frame: CGRect) {
        super.init(frame: frame)
        contentView.addSubview(cardView)
        cardView.addSubview(titleLabel)
        cardView.addSubview(subtitleLabel)
        // Layout omitted for brevity
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
    }

    func configure(with viewModel: ProtocolObjCViewModel) {
        guard self.viewModel !== viewModel else { return }
        self.viewModel = viewModel
        titleLabel.text = viewModel.model.title
        subtitleLabel.text = viewModel.model.subtitle

        let contentViewType = ProtocolObjCViewUtils.contentViewClassForVariant(viewModel.model.variant)
        if let unwrappedContentView = nuxContentView {
            if type(of: unwrappedContentView) != contentViewType {
                unwrappedContentView.removeFromSuperview()
                nuxContentView = nil
            }
        }
        if nuxContentView == nil {
            let newContentView = ProtocolObjCViewUtils.contentViewForVariant(viewModel.model.variant)
            cardView.addSubview(newContentView)
            nuxContentView = newContentView
            nuxContentView?.delegate = self
        }
        nuxContentView?.configure(with: viewModel)
    }
}

@MainActor
func test_protocol_objc_retain_cycle_bad() {
    let model = ProtocolObjCModel(
        title: "Title",
        subtitle: "Subtitle",
        cta: "CTA",
        variant: .rollDice,
        prompts: ["Prompt 1", "Prompt 2", "Prompt 3"]
    )
    let viewModel = ProtocolObjCViewModel(model: model, minimumHeight: 100)
    let cell = ProtocolObjCCell()

    // Pulse should now successfully follow the dynamically dispatched
    // `configure` call across the existential boundary to find the cycle.
    cell.configure(with: viewModel)
}
