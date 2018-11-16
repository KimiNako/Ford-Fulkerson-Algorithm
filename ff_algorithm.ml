open Graph
open Printf
    
type path = 'a list

let rec find_path graph forbidden source sink = (*if (sink=source) then forbidden else*)
	let arcs_src = out_arcs graph source in
	let rec loop arcs_src forbidden =	
		match arcs_src with
		| [] -> []
		| (id, _)::rest ->  if id=sink then (id::forbidden) else 
			let aux = find_path graph (id::forbidden) id sink in
				match aux with
					| [] -> loop rest forbidden
					| aux -> aux
	in

(* let Ford_Fulkerson_Algorithm graph source sink *)



