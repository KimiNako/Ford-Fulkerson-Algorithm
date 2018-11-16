open Graph
open Printf
    
type path = 'a list

(* before call find_path : source must be in forbidden *)
let rec find_path_bis graph forbidden source sink = (*if (sink=source) then forbidden else*)
	let arcs_src = out_arcs graph source in
	let rec loop arcs_src =	
		match arcs_src with
		| [] -> []
		| (id, _)::rest ->  if id=sink then (id::forbidden) else 
			let aux = find_path graph (id::forbidden) id sink in
				match aux with
					| [] -> loop rest 
					| aux -> aux
	in loop arcs_src
	
let find_path graph forbidden source sink = find_path_bis graph (source::forbidden) source sink


(* let Ford_Fulkerson_Algorithm graph source sink *)



