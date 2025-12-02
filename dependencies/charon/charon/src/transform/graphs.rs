use indexmap::IndexSet;
use std::collections::BTreeSet as OrdSet;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::vec::Vec;

/// A structure containing information about SCCs (Strongly Connected Components)
pub struct SCCs<Id> {
    /// The SCCs themselves
    pub sccs: Vec<Vec<Id>>,
    /// The dependencies between SCCs
    pub scc_deps: Vec<OrdSet<usize>>,
}

/// The order in which Tarjan's algorithm generates the SCCs is arbitrary,
/// while we want to keep as much as possible the original order (the order
/// in which the user generated the ids). For this, we iterate through
/// the ordered ids and try to add the SCC containing the id to a new list of SCCs
/// Of course, this SCC might have dependencies, so we need to add the dependencies
/// first (in which case we have reordering to do). This is what this function
/// does: we add an SCC and its dependencies to the list of reordered SCCs by
/// doing a depth-first search.
/// We also compute the SCC dependencies while performing this exploration.
fn insert_scc_with_deps<Id: Copy + std::hash::Hash + Eq>(
    get_id_dependencies: &dyn Fn(Id) -> Vec<Id>,
    reordered_sccs: &mut IndexSet<usize>,
    scc_deps: &mut Vec<OrdSet<usize>>,
    sccs: &Vec<Vec<Id>>,
    id_to_scc: &HashMap<Id, usize>,
    scc_id: usize,
) {
    // Check if the scc id has already been added
    if reordered_sccs.contains(&scc_id) {
        return;
    }

    // Add the dependencies.
    // For every id in the dependencies, get the SCC containing this id.
    // If this is the current SCC: continue. If it is a different one, it is
    // a dependency: add it to the list of reordered SCCs.
    let scc = &sccs[scc_id];
    for id in scc.iter() {
        let ids = get_id_dependencies(*id);
        for dep_id in ids.iter() {
            let dep_scc_id = id_to_scc.get(dep_id).unwrap();
            if *dep_scc_id == scc_id {
                continue;
            } else {
                // Insert the dependency
                scc_deps[scc_id].insert(*dep_scc_id);

                // Explore the parent SCC
                insert_scc_with_deps(
                    get_id_dependencies,
                    reordered_sccs,
                    scc_deps,
                    sccs,
                    id_to_scc,
                    *dep_scc_id,
                );
            }
        }
    }

    // Add the current SCC
    reordered_sccs.insert(scc_id);
}

/// Provided we computed the SCCs (Strongly Connected Components) of a set of
/// identifier, and those identifiers are ordered, compute the set of SCCs where
/// the order of the SCCs and the order of the identifiers inside the SCCs attempt
/// to respect as much as possible the original order between the identifiers.
/// The `ids` vector gives the ordered set of identifiers.
///
///
/// This is used in several places, for instance to generate the translated
/// definitions in a consistent and stable manner. For instance, let's
/// say we extract 4 definitions  `f`, `g1`, `g2` and `h`, where:
/// - `g1` and `g2` are mutually recursive
/// - `h` calls `g1`
///
/// When translating those functions, we group together the mutually recursive
/// functions, because they have to be extracted in one single group, and thus
/// apply Tarjan's algorithm on the call graph to find out which functions are
/// mutually recursive.
/// The implementation of Tarjan's algorithm we use gives us the Strongly Connected
/// SCCs of the call graph in an arbitrary order, so we can have:
/// `[[f], [g1, g2], [h]]`, but also `[[h], [f], [g2, g1]]`, etc.
///
/// If the user defined those functions in the order: `f, g1, h, g2`, we want
/// to reorder them into: `f, g1, g2, h`, so that we can translate the mutually
/// recursive functions together, while performing a minimal amount of reordering.
/// And if reordering is not needed, because the user defined those functions
/// in the order `f, g1, g2, h`, or `g1, g2, f, h` or ... then we want to translate
/// them in this exact order.
///
/// This function performs just that: provided the order in which the definitions
/// were defined, and the SCCs of the call graph, return an order suitable for
/// translation and where the amount of reorderings is minimal with regards to
/// the initial order.
///
/// This function is also used to generate the backward functions in a stable
/// manner: the order is provided by the order in which the user listed the
/// region parameters in the function signature, and the graph is the hierarchy
/// graph (or region subtyping graph) between those regions.
pub fn reorder_sccs<Id: std::fmt::Debug + Copy + std::hash::Hash + Eq>(
    get_id_dependencies: &dyn Fn(Id) -> Vec<Id>,
    ids: &Vec<Id>,
    sccs: &[Vec<Id>],
) -> SCCs<Id> {
    // Map the identifiers to the SCC indices
    let mut id_to_scc = HashMap::<Id, usize>::new();
    for (i, scc) in sccs.iter().enumerate() {
        for id in scc {
            id_to_scc.insert(*id, i);
        }
    }

    // Reorder the identifiers inside the SCCs.
    // We iterate over the identifiers (in the order in which we register them), and
    // add them in their corresponding SCCs.
    let mut reordered_sccs: Vec<Vec<Id>> = vec![];
    sccs.iter().for_each(|_| reordered_sccs.push(vec![]));
    for id in ids {
        let scc_id = match id_to_scc.get(id) {
            None => panic!("Could not find id: {:?}", id),
            Some(id) => id,
        };
        reordered_sccs[*scc_id].push(*id);
    }

    // Reorder the SCCs themselves - just do a depth first search. Iterate over
    // the def ids, and add the corresponding SCCs (with their dependencies).
    let mut reordered_sccs_ids = IndexSet::<usize>::new();
    let mut scc_deps: Vec<OrdSet<usize>> = reordered_sccs.iter().map(|_| OrdSet::new()).collect();
    for id in ids {
        let scc_id = id_to_scc.get(id).unwrap();
        insert_scc_with_deps(
            get_id_dependencies,
            &mut reordered_sccs_ids,
            &mut scc_deps,
            &reordered_sccs,
            &id_to_scc,
            *scc_id,
        );
    }
    let reordered_sccs_ids: Vec<usize> = reordered_sccs_ids.into_iter().collect();

    // Compute the map from the original SCC ids to the new SCC ids (after
    // reordering).
    let mut old_id_to_new_id: Vec<usize> = reordered_sccs_ids.iter().map(|_| 0).collect();
    for (new_id, dep) in reordered_sccs_ids.iter().enumerate() {
        old_id_to_new_id[*dep] = new_id;
    }

    // Generate the reordered SCCs
    let tgt_sccs = reordered_sccs_ids
        .iter()
        .map(|scc_id| reordered_sccs[*scc_id].clone())
        .collect();

    // Compute the dependencies with the new indices
    let mut tgt_deps: Vec<OrdSet<usize>> = reordered_sccs.iter().map(|_| OrdSet::new()).collect();
    for (old_id, deps) in scc_deps.iter().enumerate() {
        let new_id = old_id_to_new_id[old_id];
        tgt_deps[new_id] = OrdSet::from_iter(deps.iter().map(|old| old_id_to_new_id[*old]));
    }

    SCCs {
        sccs: tgt_sccs,
        scc_deps: tgt_deps,
    }
}
