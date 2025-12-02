//! Defines some utilities for [crate::names]
//!
//! For now, we have one function per object kind (type, trait, function,
//! module): many of them could be factorized (will do).
use crate::ast::*;

impl PathElem {
    fn equals_ident(&self, id: &str) -> bool {
        match self {
            PathElem::Ident(s, d) => s == id && d.is_zero(),
            _ => false,
        }
    }
}

impl Name {
    /// Convert a path like `["std", "alloc", "Box"]` to a name. Needed on occasion when crafting
    /// names that were not present in the original code.
    pub(crate) fn from_path(path: &[&str]) -> Name {
        Name {
            name: path
                .iter()
                .map(|elem| PathElem::Ident(elem.to_string(), Disambiguator::ZERO))
                .collect(),
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.name.len()
    }

    /// If this item comes from monomorphization, return the arguments used.
    pub fn mono_args(&self) -> Option<&GenericArgs> {
        Some(self.name.last()?.as_monomorphized()?.as_ref())
    }

    /// Compare the name to a constant array.
    /// This ignores disambiguators.
    ///
    /// `equal`: if `true`, check that the name is equal to the ref. If `false`:
    /// only check if the ref is a prefix of the name.
    pub fn compare_with_ref_name(&self, equal: bool, ref_name: &[&str]) -> bool {
        let name: Vec<&PathElem> = self.name.iter().filter(|e| e.is_ident()).collect();

        if name.len() < ref_name.len() || (equal && name.len() != ref_name.len()) {
            return false;
        }

        for i in 0..ref_name.len() {
            if !name[i].equals_ident(ref_name[i]) {
                return false;
            }
        }
        true
    }

    /// Compare the name to a constant array.
    /// This ignores disambiguators.
    pub fn equals_ref_name(&self, ref_name: &[&str]) -> bool {
        self.compare_with_ref_name(true, ref_name)
    }
}
