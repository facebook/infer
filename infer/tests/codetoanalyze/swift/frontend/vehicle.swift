class Vehicle {
    func makeNoise() {
        // do nothing - an arbitrary vehicle doesn't necessarily make a noise
    }
}
class Train: Vehicle {
    override func makeNoise() {
        //print("Choo Choo")
    }
}
func main(_ v: Vehicle, _ t: Train) {
     v.makeNoise()
}
