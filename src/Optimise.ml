(*w
  ====Optimisation passes====
  This module handles the various Javascript optimisation passes. These passes
  must of type ^^passes^^. Programm options to enable and disable individually
  each pass are automatically generated.
*)
open Arg
type specs=(key * spec * doc) list

class type pass=
object
 method run : AstJs.program -> AstJs.program
 method name : string
 method description : string
end

class type optimisation=
object
 inherit pass
 method spec : specs
end

let optDisabled=ref false

let dname f=
 ("-disable-"^f#name)

let control f=
 let disable=ref false
 and run=f#run in
 let run p=
  if !disable || !optDisabled then
   p
  else
   run p
 and spec=dname f,
 (Arg.Unit (fun () -> disable:=true)),
 "Disable "^f#description
 in run,spec

class opt (l:#pass list): optimisation=
 let runPasses,spec=List.split (List.map control l)
 and passopt=
  let passFlags = List.map (fun f->(dname f)^":"^(f#name))  l in
  fun () -> List.iter print_endline passFlags; exit 0
 in
object
 method run=List.fold_left (fun f g -> fun x -> g (f x) ) (fun x -> x) runPasses
 method name="Optimisations"
 method spec=
  ("-noopt",(Arg.Unit (fun () -> optDisabled:=true)),
   "Disable all the optimisation passes")
  ::("-shoptpasses",Arg.Unit passopt,"<undocumented>")
  ::spec
 method description="Optimisations"
end
let make l=
 new opt l
