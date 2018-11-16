(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

(* A path is a list of nodes. *)
type path = id list

(* find_path graph [] source sink *)
(* Find a path beetween source and sink *)
(* Return [] if no path found *)
val find_path: int graph -> id list -> id -> id -> path


