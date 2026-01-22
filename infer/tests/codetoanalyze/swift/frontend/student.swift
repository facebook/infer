class Student {
    let age: Int
    var weight: Int
    init(age: Int, weight: Int) {
        self.age = age
        self.weight = weight
    }
}

func create_student() -> Int {
    return Student(age: 30, weight: 60).age
}

func test_student_allocation_bad() {
    assert(create_student() == 35)
}

func test_student_allocation_good() {
    assert(create_student() == 30)
}


func setWeight( _ weight: Int) {
    let john = Student(age: 30, weight: 60)
    john.weight = weight
}

struct Employee {
    let age: Int
    private var _weight: Int
    var weight: Int {
        get { _weight }
        set { _weight = newValue }
    }
    init(age: Int, weight: Int) {
        self.age = age
        self._weight = weight
    }
}

func create_employee() -> Int {
    return Employee(age: 30, weight: 60).age
}

func test_employee_allocation_bad() {
    assert(create_employee() == 35)
}

func test_employee_allocation_good() {
    assert(create_employee() == 30)
}


func setEmployeeWeight( _ weight: Int) -> Int {
    var john = Employee(age: 30, weight: 60)
    john.weight = weight
    return john.weight
}

func test_employee_getter_setter_bad() {
    assert(setEmployeeWeight(70) == 75)
}

func test_employee_getter_setter_good() {
    assert(setEmployeeWeight(70) == 70)
}

class Person {
    let age: Int
    var spouse: Person
    init(age: Int) {
        self.age = age
        self.spouse = Person(age: 0)
    }

    func setting_jane(_ jane: Person) {
        self.spouse = jane
    }
}

func test_retain_cycle_bad(_ john: Person, _ jane: Person) {
    john.spouse = jane
    jane.spouse = john
}

func createPerson() -> Person {
    fatalError("Fail here")
}


func test_retain_cycle_person1_bad(_ jane: Person) {
    let john = createPerson()
    john.spouse = jane
    jane.spouse = john
}



class Individual {
    let age: Int
    weak var spouse: Individual?
    init(age: Int) {
        self.age = age
    }
}

func set_spouses(_ john: Individual, _ jane: Individual) {
    john.spouse = jane
    jane.spouse = john
}

func test_retain_cycle1_bad() {
    let john = Individual(age: 30)
    let jane = Individual(age: 35)
    set_spouses(john, jane)
}

func test_optional(_ age : Int?) -> Int {
    if let actualAge = age {
        return actualAge
    }
    else {
        return 0
    }
}

func test_optional_good_fp() {
    assert(test_optional(30) == 30)
}

func test_optional_bad() {
    assert(test_optional(30) == 35)
}

func test_optional_person(_ person : Person?) -> Int {
    if let actualPerson = person {
        return actualPerson.age
    }
    else {
        return 0
    }
}

func test_optional_person_good() -> Int {
    test_optional_person(Person(age: 30))
}

func test_optional_preson_nil_good() -> Int {
    test_optional_person(nil)
}

// This reports an assertion error, but in prod it would just create
// only one spec with person <> nil. In that case test_optional3_bad_FN
// should report an NPE but it doesn't yet.
func test_optional_crash_bad(_ person: Person?) -> Int {
    return person!.age // This will crash if age is nil!
}

func test_optional3_bad_FN() -> Int {
    test_optional_crash_bad(nil)
}

class ViewController {
    var view: CustomView?

    init() {
        self.view = CustomView(delegate: self)
    }
}

class CustomView {
    var delegate: ViewController

    init(delegate: ViewController) {
        self.delegate = delegate
    }
}

func retainCycleExample() {
    let _ = ViewController()
}

class RetainCycleExample {
    var id = 10
    // The closure property
    var closure: (() -> Void)?
    func setupClosureBad() {
        // Capturing self strongly inside the closure
        closure = {
            self.id = 20
        }
    }

     func setupClosureOk() {
       closure = { [weak self] in
            self?.id = 20
        }
    }
}

final class State {
    var delegate: DeviceAppManagerClientDelegate?
}

protocol DeviceAppManagerClientDelegate: AnyObject {}

final class DeviceAppManagerDelegateImpl: DeviceAppManagerClientDelegate {
    let onStartServiceResponse: () -> Void
    init(onStartServiceResponse: @escaping () -> Void) {
        self.onStartServiceResponse = onStartServiceResponse
    }
}

func foo(_ state : State?) {}

func test_retain_cycle_bad2() {
    // Set up state and a reference to it
    let state = State()
    var stateRef: State? = state

    // Create the delegate, capturing stateRef in the closure
    let delegate = DeviceAppManagerDelegateImpl {
        // This closure captures stateRef, creating a retain cycle
        stateRef = nil
    }
    foo(stateRef)

    state.delegate = delegate
}

enum Day {
    case monday
    case tuesday
    case wednesday
    case thursday
    case friday
    case saturday
    case sunday
}

func isWeekend(_ day: Day) -> Bool {
    return day == .saturday || day == .sunday
}

func using_enums_with_thursday_bad() {
    let today = Day.thursday
    assert(isWeekend(today) == true) //assertion error reported here because it's false
}

func using_enums_with_saturday_bad() {
    let today = Day.saturday
    assert(isWeekend(today) == false) //assertion error reported here because it's true
}

func using_enums_with_sunday_bad() {
    let today = Day.sunday
    assert(isWeekend(today) == false) //assertion error reported here because it's true
}

func using_enums_with_thursday_ok() {
    let today = Day.thursday
    assert(isWeekend(today) == false)
}

func using_enums_with_saturday_ok() {
    let today = Day.saturday
    assert(isWeekend(today) == true)
}

// Example that shows we do not crash on complex enums
// We haven't yet checked that we translate correctly the semantics of this code,
// but it doesn't crash and it doesn't have typechecking errors.
enum CoverPictureModel: Equatable {
    case empty
    case upload(imageName: String)
}
class Profile {
    public private(set) var coverPictureModel: CoverPictureModel = .empty {
        didSet {
            guard coverPictureModel != oldValue else { return }
            updateCollectionViewForItems(["coverPicture"])
        }
    }
    func updateCollectionViewForItems(_ items: [String]) {
        print("Updating collection view for items: \(items)")
    }
}

struct Quality: Equatable {
    let value: Int
    let sndValue: Int
}

func retain_cycle_variable_shadow_fp(quality: Int) -> Quality {
    let quality = Quality(value: quality, sndValue: 0)
    return quality
}
