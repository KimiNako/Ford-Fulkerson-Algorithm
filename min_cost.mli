open Graph
open Ff_algorithm
type infile = string
type outfile = string
type capacity = int
type flow = int
type cost = int


val create_assignement_graph : string -> (capacity*cost) graph

(* From an assignment graph, return a flow graph with the maximum flow and the minimum cost *)
val busaker_gowen_algorithm : ((capacity * cost) graph * id * id) -> ((capacity * flow * cost) graph * flow * cost)

(* From a file which contains a problem of assignments, find a solution with a the maximum flow and the minimum cost (the outfile is in dot format)*)
val min_cost_algorithm : infile -> outfile -> unit
