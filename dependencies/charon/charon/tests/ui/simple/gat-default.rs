//@ known-failure
//! Test associated type defaults with parameters
#![feature(associated_type_defaults)]

trait Collection {
    type Sibling<U> = Vec<U>;
}

impl Collection for () {}

fn main() {}
