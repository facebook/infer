pub trait Hasher {}

pub struct DefaultHasher;
impl Hasher for DefaultHasher {}

pub trait Hash {
    fn hash<H: Hasher>(&self, state: &mut H);
}

impl Hash for u32 {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

fn main() {
    let mut hasher = DefaultHasher;
    0u32.hash(&mut hasher);
}
