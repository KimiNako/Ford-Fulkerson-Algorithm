(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

(* A path is a list of nodes. *)
type path = id list

type capacity
type value
type flow
type flow_graph

(* find_path graph [] source sink *)
(* Find a path beetween source and sink *)
(* Return [] if no path found *)
val find_path: flow_graph -> id list -> id -> id -> path


(* Return the smallest label among all labels of a given path from the residual graph *)
(* Find the incrementation of flow *)
val find_min_arc : flow_graph -> id list -> value -> value

val ford_fulkerson_algorithm : 'a graph -> id -> id -> flow
