type id = string

type 'a out_arcs = (id * 'a) list

(* A graph is just a list of pairs: a node & its outgoing arcs. *)
type 'a graph = (id * 'a out_arcs) list

exception Graph_error of string

let empty_graph = []

let node_exists gr id = List.mem_assoc id gr

let out_arcs gr id =
  try List.assoc id gr
  with Not_found -> raise (Graph_error ("Node " ^ id ^ " does not exist in this graph."))

let find_arc gr id1 id2 =
  let out = out_arcs gr id1 in
  try Some (List.assoc id2 out)
  with Not_found -> Printf.printf("here");None

let add_node gr id =
  if node_exists gr id then raise (Graph_error ("Node " ^ id ^ " already exists in the graph."))
  else (id, []) :: gr

let add_arc gr id1 id2 lbl =

  (* Existing out-arcs *)
  let outa = out_arcs gr id1 in

  (* Update out-arcs.
   * remove_assoc does not fail if id2 is not bound.  *)
  let outb = (id2, lbl) :: List.remove_assoc id2 outa in
  
  (* Replace out-arcs in the graph. *)
  let gr2 = List.remove_assoc id1 gr in
  (id1, outb) :: gr2

let v_iter gr f = List.iter (fun (id, out) -> f id out) gr

let v_fold gr f acu = List.fold_left (fun acu (id, out) -> f acu id out) acu gr

(* Return a list of nodes for a given graph*)
let rec create_nodes acu gr = match gr with
  | []-> acu
  | (id, out_arc)::rest -> create_nodes (add_node acu id)  rest

(* Return a list of arcs
 * Apply function f on their labels *)
let rec create_arcs acu id1 listarcs f = match listarcs with
  | []-> acu
  | (id2, cost)::rest -> create_arcs (add_arc acu id1 id2 (f cost)) id1 rest f

(* Apply function f to every label of the graph*) 
let map gr f = 
  let gr2 = create_nodes [] gr 
  in
  let rec loop acu gr f =
       match gr with
       | [] -> acu
       | (id1, listarcs)::rest -> loop (create_arcs acu id1 listarcs f) rest f
  in
  loop gr2 gr f 

