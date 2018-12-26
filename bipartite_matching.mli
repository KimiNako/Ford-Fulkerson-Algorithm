
open Graph
open Ff_algorithm
type infile = string
type outfile = string
(*
val bipartite_matching_algorithm : infile -> outfile -> ()
*)

val read_node : (int*int) graph -> string -> bool -> (int*int) graph

val read_arc : (int*int) graph -> string ->  (int*int) graph

val create_bipartite_graph : string -> (int*int) graph
