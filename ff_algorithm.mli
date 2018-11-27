(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

(* A path is a list of nodes. *)
type path = id list


(*
type problem = (int graph * source * sink)
type solution = (flow_graph * max_flow) *)


(* Return the flow graph with the maximum of flow *)

val ford_fulkerson_algorithm : (int graph * id * id) -> ((int * int) graph * int)


