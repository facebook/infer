import Foundation

// Delegate protocol for message receiver
protocol MessageReceiverDelegate: AnyObject {
    func didReceiveMessage(_ message: String)
}
// Message receiver class
class MessageReceiver {
    // Intentionally NOT weak to demonstrate retain cycle risk
    var delegate: MessageReceiverDelegate?
}

// Delegate protocol for transport
protocol TransportDelegate: AnyObject {
    func didTransitionTo(state: String, transport: Transport)
}
// Transport class, which is also the delegate for MessageReceiver
class Transport: MessageReceiverDelegate {
    enum ConnectionState: String {
        case connected, disconnected
    }
    var connected: ConnectionState = .connected
    var messageReceiver: MessageReceiver
    var delegate: TransportDelegate?
    init() {
        messageReceiver = MessageReceiver()
        messageReceiver.delegate = self
    }
    public func onDisconnected() {
        connected = .disconnected
        // This sequence can create a retain cycle if delegate is not weak
        messageReceiver.delegate = nil
        messageReceiver = MessageReceiver()
        messageReceiver.delegate = self
        delegate?.didTransitionTo(state: connected.rawValue, transport: self)
    }
    // Delegate method implementation
    func didReceiveMessage(_ message: String) {
        print("Received message: \(message)")
    }
}
