
open Graph
open Ff_algorithm
type infile = string
type outfile = string
(*
val bipartite_matching_algorithm : infile -> outfile -> ()
*)

val read_node : int graph -> string -> bool -> int graph

val read_arc : int graph -> string ->  int graph

val create_bipartite_graph : string -> int graph

val bipartite_matching_algorithm : string -> string -> unit
