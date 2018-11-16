open Graph
    
type path = id list

let rec find_path graph path source sink = 
	Printf.printf "noeud %s \n" source;
	if (sink=source) then (source::path) else
	let arcs_src = out_arcs graph source in
	let rec loop arcs_src =	
		match arcs_src with
		| [] -> []
		| (id, _)::rest -> 
	if (List.exists (fun x -> (x=id)) path) then loop rest
	else let aux = find_path graph (source::path) id sink in
				match aux with
					| [] -> loop rest 
					| aux -> aux
	in loop arcs_src

(* let Ford_Fulkerson_Algorithm graph source sink *)

