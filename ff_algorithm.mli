(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

(* A path is a list of nodes. *)
type path = id list


val find_path : ('a * int) graph -> path -> id -> id -> path

val find_min_arc : ('a*int) graph -> path -> int -> int

val decremente : ('a*int) graph -> id -> id -> int -> ('a*int) graph

(* Initialize flow or residual graph *)
val init_flow_graph : 'a graph -> ('a*int) graph
val init_residual_graph : 'a graph -> ('a*'a) graph

(* Update flow or residual graph *)
val update_flow_graph : ('a*int) graph -> path -> int -> ('a*int) graph 
val update_residual_graph : (int*int) graph -> path -> int -> (int*int) graph 

(* Calculate max flow of a given graph *)
val calculate_max_flow : ('a*int) graph -> id -> int


(* Return the flow graph with the maximum of flow *)
val ford_fulkerson_algorithm : (int graph * id * id) -> ((int * int) graph * int)


