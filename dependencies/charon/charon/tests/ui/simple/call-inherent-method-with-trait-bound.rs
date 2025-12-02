//@ charon-args=--remove-associated-types=*
pub trait Trait {
    type Type;
}

impl Trait for () {
    type Type = ();
}

pub struct HashMap<S>(S);

impl<S> HashMap<S> {
    pub fn get<Q: Trait>(_x: HashMap<S>, _k: Q) {}
}

pub fn top_level_get<S, Q: Trait>(_x: HashMap<S>, _k: Q) {}

pub fn test1(map: HashMap<()>) {
    top_level_get(map, ());
}
pub fn test2(map: HashMap<()>) {
    HashMap::get(map, ());
}
