use std::fmt::Debug;
use std::mem;
use std::vec::Vec;

use itertools::Itertools;
use petgraph::algo::scc::tarjan_scc;
use petgraph::graphmap::NodeTrait;
use petgraph::prelude::DiGraphMap;
use petgraph::visit::{DfsPostOrder, Walker};

use crate::ast::*;

/// Compute the SCCs (Strongly Connected Components) of a set of identifiers, where the order of
/// the SCCs and the order of the identifiers inside the SCCs attempt to respect as much as
/// possible the sort order given. The provided `sort_by` should be cheap.
///
/// This is used to generate the translated definitions in a consistent and stable manner. For
/// instance, let's say we extract 4 definitions  `f`, `g1`, `g2` and `h`, where:
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
/// The order in which Tarjan's algorithm generates the SCCs is arbitrary, while we want to keep as
/// much as possible the original order (the order in which the user generated the ids). For this,
/// we iterate through the ordered ids and try to add the SCC containing the id to a new list of
/// SCCs Of course, this SCC might have dependencies, so we need to add the dependencies first (in
/// which case we have reordering to do). This is what this function does: we add an SCC and its
/// dependencies to the list of reordered SCCs by doing a depth-first search.
pub fn ordered_scc<Id: NodeTrait + Debug, O: Ord>(
    graph: &DiGraphMap<Id, ()>,
    sort_by: impl Fn(&Id) -> O,
) -> Vec<Vec<Id>> {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct SccId(usize);

    let mut sccs = tarjan_scc(graph);

    // Reorder the identifiers inside the SCCs.
    for scc in sccs.iter_mut() {
        scc.sort_by_key(&sort_by);
    }

    // Map the nodes to their SCC index. The map is sorted.
    let id_to_scc: SeqHashMap<Id, SccId> = sccs
        .iter()
        .enumerate()
        .map(|(i, nodes)| (SccId(i), nodes))
        .flat_map(|(scc_id, scc_nodes)| scc_nodes.iter().map(move |node| (*node, scc_id)))
        .sorted_by_key(|(node, _)| sort_by(node))
        .collect();

    // Compute the graph of the sccs, where there is an edge between sccs if there's an edge
    // between some nodes of each scc.
    let mut scc_graph: DiGraphMap<SccId, ()> = DiGraphMap::new();
    // Add sccs in the order of their earliest element.
    for (_, &scc_id) in &id_to_scc {
        scc_graph.add_node(scc_id);
    }
    for (scc_id, scc_nodes) in sccs.iter().enumerate().map(|(i, nodes)| (SccId(i), nodes)) {
        // Add the scc neighbors in the desired node order.
        for neighbor_scc_id in scc_nodes
            .iter()
            .flat_map(|node| graph.neighbors(*node))
            .sorted_by_key(&sort_by)
            .map(|neighbor| *id_to_scc.get(&neighbor).unwrap())
        {
            scc_graph.add_edge(scc_id, neighbor_scc_id, ());
        }
    }

    // Reorder the SCCs among themselves by a post-order visit of the graph: for each scc (in the
    // order of their earliest node in the desired order), we list it and all the sccs it depends
    // on we haven't yet included, in postorder. This guarantees that the final order is a
    // topological sort for the scc DAG. Moreover this will explore other sccs in order too because
    // we sorted the list of neighbors.
    let mut reordered_sccs_ids: SeqHashSet<SccId> = SeqHashSet::new();
    for scc_id in scc_graph.nodes() {
        for scc_id in DfsPostOrder::new(&scc_graph, scc_id).iter(&scc_graph) {
            reordered_sccs_ids.insert(scc_id);
        }
    }

    // Output the fully reordered SCCs.
    reordered_sccs_ids
        .into_iter()
        .map(|scc_id| mem::take(&mut sccs[scc_id.0]))
        .collect()
}

#[cfg(test)]
mod tests {
    use petgraph::prelude::DiGraphMap;

    #[test]
    fn test_reorder_sccs1() {
        let mut graph = DiGraphMap::new();

        // First nontrivial cc
        graph.add_edge(1, 2, ());
        graph.add_edge(2, 1, ());
        // Second nontrivial cc
        graph.add_edge(3, 4, ());
        graph.add_edge(4, 5, ());
        graph.add_edge(5, 3, ());
        // Other deps
        graph.add_edge(1, 0, ());
        graph.add_edge(0, 3, ());
        graph.add_edge(1, 3, ());

        let reordered = super::ordered_scc(&graph, |i| *i);

        assert_eq!(reordered, vec![vec![3, 4, 5], vec![0], vec![1, 2],]);
    }
}
