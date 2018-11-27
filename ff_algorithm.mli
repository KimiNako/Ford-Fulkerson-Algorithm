(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

(* A path is a list of nodes. *)
type path = id list
type source = id
type sink =id

type capacity
type value
type max_flow = int

type flow_graph = (capacity * value) graph

(*
type problem = (int graph * source * sink)
type solution = (flow_graph * max_flow) *)


(* Return the flow graph with the maximum of flow *)
val ford_fulkerson_algorithm : (int graph * source * sink) -> (flow_graph * max_flow)


