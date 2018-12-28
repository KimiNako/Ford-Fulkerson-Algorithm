open Graph
open Ff_algorithm
type infile = string
type outfile = string

val read_node : int graph -> string -> bool -> int graph

val read_arc : int graph -> string ->  int graph

val create_bipartite_graph : string -> int graph

(* From a file which contains bipartite graph, find a solution (the outfile is in dot format)*)
val bipartite_matching_algorithm : infile -> outfile -> unit
