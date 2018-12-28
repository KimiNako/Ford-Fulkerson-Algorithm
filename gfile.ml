open Graph
open Printf
    
type path = string

(* Format of text files: lines of the form 
 *
 *  v id               (node with the given identifier)
 *  e label id1 id2    (arc with the given (string) label. Goes from node id1 to node id2.)
 *
 *)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out ("Tests/"^path) in

  (* Write in this file. *)
  fprintf ff "=== Graph file ===\n\n" ;

  (* Write all nodes *)
  v_iter graph (fun id _ -> fprintf ff "v %s\n" id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  v_iter graph (fun id out -> List.iter (fun (id2, lbl) -> fprintf ff "e \"%s\" %s %s\n" lbl id id2) out) ;
  
  fprintf ff "\n=== End of graph ===\n" ;
  
  close_out ff ;
  ()

(* Read a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "v %s" (fun id -> add_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

(* Read a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e \"%s@\" %s %s" (fun label id1 id2 -> add_arc graph id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : v or e.
         * Lines not starting with v or e are ignored. *)
        else match line.[0] with
          | 'v' -> read_node graph line
          | 'e' -> read_arc graph line
          | _ -> graph
      in                 
      loop graph2        
    with End_of_file -> graph
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph
  
(* Return an arc in the following format: id1 -> id2 [ label = value-of-label ]; *) 
let write_arc idsrc iddest lbl = idsrc^"->"^iddest^" [ label = \""^lbl^"\" ];\n"

(* Return a list of arcs in the following format: id1 -> id2 [ label = value-of-label ]; *) 
let rec looparc acu id outarcs =
	match outarcs with
	| [] -> acu
	| (iddest, lbl) :: rest -> looparc ((write_arc id iddest lbl)^acu) id rest 

(* Export/Write a graph in a file*) 
let export gr path = 
	let ff= open_out ("Tests/GV_files/"^path) in
	let arcs = v_fold gr looparc "" in
	fprintf ff "digraph finite_state_machine {\n
	rankdir=LR;\n
	size=\"8,5\"\n
	node [shape = circle];\n %s\n}\n" arcs







