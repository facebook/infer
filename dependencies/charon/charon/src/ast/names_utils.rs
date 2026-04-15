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

    pub fn as_monomorphized(&self) -> Option<&GenericArgs> {
        let binder = self.as_instantiated()?;
        binder.params.is_empty().then_some(&binder.skip_binder)
    }
    pub fn is_monomorphized(&self) -> bool {
        self.as_monomorphized().is_some()
    }
}

impl Name {
    /// Convert a path like `["std", "alloc", "Box"]` to a name. Needed on occasion when crafting
    /// names that were not present in the original code.
    pub fn from_path(path: &[&str]) -> Name {
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
        Some(self.name.last()?.as_monomorphized()?)
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

    /// Created an instantiated version of this name by putting a `PathElem::Instantiated` last. If
    /// the item was already instantiated, this merges the two instantiations.
    pub fn instantiate(mut self, binder: Binder<GenericArgs>) -> Self {
        if let [.., PathElem::Instantiated(box x)] = self.name.as_mut_slice() {
            // Put the new args in place; the params are what we want but the args are wrong.
            let old_args = std::mem::replace(x, binder);
            // Apply the new args to the old binder to get correct args.
            x.skip_binder = old_args.apply(&x.skip_binder);
        } else {
            self.name.push(PathElem::Instantiated(Box::new(binder)));
        }
        self
    }
}
