open Unix;;
let fork_exec args = 
	let pid = Unix.fork () in
		if pid=0 then  (* -- Code of the child *)
  		ignore(Unix.system args) 
		else 
		(* waiting optional, removing this part makes llvm code easier probs *)
			match wait () with
      	| (pid, WEXITED retcode) -> ignore()
      	| (pid, _)               -> failwith "false script" in
fork_exec "for((i=1;i<=100;i++))
do
    echo $i
done";;
