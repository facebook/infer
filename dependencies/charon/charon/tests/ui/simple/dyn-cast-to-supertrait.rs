trait Feline {
    fn hunt(&self);
}

trait Pettable {
    fn pet(&self);
}

trait Cat: Feline + Pettable {
    fn meow(&self);
}

struct HouseCat;
impl Feline for HouseCat {
    fn hunt(&self) {}
}
impl Pettable for HouseCat {
    fn pet(&self) {}
}
impl Cat for HouseCat {
    fn meow(&self) {}
}

fn main() {
    let cat = HouseCat;
    let dyn_cat: &dyn Cat = &cat;
    let dyn_feline: &dyn Feline = dyn_cat;
    let dyn_pettable: &dyn Pettable = dyn_cat;
}
