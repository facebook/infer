class BaseView {
    var id: Int = 0
}

class DerivedViewA: BaseView {
    func doA() {}
}

class DerivedViewB: BaseView {
    func doB() {}
}

struct ClassUtils {
    // Returns the metatype for DerivedViewA
    static func viewClassForVariant() -> BaseView.Type {
        return DerivedViewA.self
    }
}

final class ClassCell {
    // Held as an optional base class pointer
    var baseView: BaseView?
}

// MARK: - Tests for Matching Types

func test_class_type_of_same_type_bad() {
    let cell = ClassCell()
    cell.baseView = DerivedViewA()
    let expectedType = ClassUtils.viewClassForVariant()

    // Unwrapped! DerivedViewA.self != DerivedViewA.self -> False (Assertion fails / Bad)
    assert (type(of: cell.baseView!) != expectedType)
}

func test_class_type_of_same_type_good() {
    let cell = ClassCell()
    cell.baseView = DerivedViewA()
    let expectedType = ClassUtils.viewClassForVariant()

    // Unwrapped! DerivedViewA.self == DerivedViewA.self -> True (Good)
    assert(type(of: cell.baseView!) == expectedType)
}

// MARK: - Tests for Different Types

func test_class_type_of_different_types_bad() {
    let cell = ClassCell()
    cell.baseView = DerivedViewA()
    let expectedType = DerivedViewB.self

    // Unwrapped! DerivedViewA.self == DerivedViewB.self -> False (Assertion fails / Bad)
    assert(type(of: cell.baseView!) == expectedType)
}

func test_class_type_of_different_types_good() {
    let cell = ClassCell()
    cell.baseView = DerivedViewA()
    let expectedType = DerivedViewB.self

    // Unwrapped! DerivedViewA.self != DerivedViewB.self -> True (Good)
    assert(type(of: cell.baseView!) != expectedType)
}

// MARK: - Tests through `AnyObject`
//
// When the static type is erased to `AnyObject`, swiftc cannot lift the
// metadata read to a direct field load and emits the runtime helper
// `swift_getObjectType` instead.  Without a Pulse model for that helper
// the result register has no dynamic type and `__swift_metadata_equals`
// returns an unconstrained boolean — both branches are explored and
// every assertion site fires `PULSE_ASSERTION_ERROR`, including the
// `_good` cases (false positives).  The next diff in the stack adds the
// `swift_getObjectType` model and drops the FPs.

func test_anyobject_type_of_same_type_bad() {
    let x: AnyObject = DerivedViewA()
    // type(of: x) == DerivedViewA.self -> True, so the != is False (Assertion fails / Bad)
    assert(type(of: x) != DerivedViewA.self)
}

func test_anyobject_type_of_same_type_good_FP() {
    let x: AnyObject = DerivedViewA()
    // type(of: x) == DerivedViewA.self -> True (Good)
    assert(type(of: x) == DerivedViewA.self)
}

func test_anyobject_type_of_different_types_bad() {
    let x: AnyObject = DerivedViewA()
    // type(of: x) == DerivedViewB.self -> False (Assertion fails / Bad)
    assert(type(of: x) == DerivedViewB.self)
}

func test_anyobject_type_of_different_types_good_FP() {
    let x: AnyObject = DerivedViewA()
    // type(of: x) != DerivedViewB.self -> True (Good)
    assert(type(of: x) != DerivedViewB.self)
}
