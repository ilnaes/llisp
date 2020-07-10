use crate::expr::expr::*;
use crate::types::get_free;
use im::{HashMap, HashSet};
use petgraph::{
    algo::tarjan_scc,
    graph::{Graph, NodeIndex},
};

// get groups of mutually recursive functions using tarjan's algorithm
pub fn scc<'a, 'b>(prog: &'b [Def<'a>]) -> Vec<Vec<&'b Def<'a>>> {
    let mut graph: Graph<&'b Expr<'a>, ()> = Graph::new();
    let mut node = HashMap::new();
    let mut back = HashMap::new();
    let mut visited: HashSet<&'b Expr<'a>> = HashSet::new();

    for f in prog.iter() {
        let Def::FuncDef(s, _, _) = f;

        // map name to actual definition
        let n = graph.add_node(s);
        node.insert(s, n);
        back.insert(n, f);
    }

    // build call graph
    for f in prog.iter() {
        let Def::FuncDef(s, args, body) = f;

        if let Expr::EId(name) = s {
            let mut scope = HashSet::new();
            scope.insert(*name);

            for a in args.iter() {
                if let Expr::EId(v) = a {
                    scope.insert(*v);
                }
            }
            build_graph(s, body, &mut graph, &mut visited, scope, node.clone());
        }
    }

    // eprintln!("{:?}\n", petgraph::dot::Dot::new(&graph));

    // convert nodeindex back to defs
    tarjan_scc(&graph)
        .iter()
        .map(|x| x.iter().map(|y| *back.get(&y).unwrap()).collect())
        .collect()
}

fn build_graph<'a, 'b>(
    src: &'b Expr<'a>,
    e: &'b Expr<'a>,
    graph: &mut Graph<&'b Expr<'a>, ()>,
    visited: &mut HashSet<&'b Expr<'a>>,
    mut scope: HashSet<&'a str>,
    node_idx: HashMap<&'b Expr<'a>, NodeIndex<u32>>,
) {
    match e {
        Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EId(s) => {
            if !scope.contains(s) {
                // free variables in functions are function calls
                graph.add_edge(
                    *node_idx
                        .get(src)
                        .expect(&format!("{:?} -- {:?}", src, node_idx)),
                    *node_idx
                        .get(e)
                        .expect(&format!("{:?} -- {:?}", e, node_idx)),
                    (),
                );
            }
        }
        Expr::EPrint(e) => {
            build_graph(src, e, graph, visited, scope, node_idx);
        }
        Expr::EPrim2(_, e1, e2) => {
            build_graph(src, e1, graph, visited, scope.clone(), node_idx.clone());
            build_graph(src, e2, graph, visited, scope, node_idx);
        }
        Expr::EIf(c, e1, e2) => {
            build_graph(src, c, graph, visited, scope.clone(), node_idx.clone());
            build_graph(src, e1, graph, visited, scope.clone(), node_idx.clone());
            build_graph(src, e2, graph, visited, scope, node_idx);
        }
        Expr::ETup(args) => {
            for a in args.iter() {
                build_graph(src, a, graph, visited, scope.clone(), node_idx.clone());
            }
        }
        Expr::EApp(f, args) => {
            build_graph(src, f, graph, visited, scope.clone(), node_idx.clone());
            for a in args.iter() {
                build_graph(src, a, graph, visited, scope.clone(), node_idx.clone());
            }
        }
        Expr::ELet(binds, body) => {
            let mut sc = scope.clone();
            for Binding(x, e) in binds.iter() {
                build_graph(src, e, graph, visited, scope.clone(), node_idx.clone());
                if let Expr::EId(s) = x {
                    sc.insert(s);
                }
            }
            build_graph(src, body, graph, visited, sc, node_idx.clone());
        }
        Expr::ELambda(_, args, body) => {
            graph.add_edge(*node_idx.get(src).unwrap(), *node_idx.get(e).unwrap(), ());

            if visited.contains(e) {
                return;
            }
            visited.insert(e);

            for a in args.iter() {
                if let Expr::EId(s) = a {
                    scope.insert(s);
                }
            }

            let mut free = Vec::new();
            get_free(e, im::HashSet::new(), &mut free);
            if free.len() > 0 {
                graph.add_edge(*node_idx.get(e).unwrap(), *node_idx.get(src).unwrap(), ());
            }

            build_graph(e, body, graph, visited, scope, node_idx);
        }
    }
}
