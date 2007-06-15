(*w
  This script converts the output of ^^darcs -changes^^ to the ^^lp4all^^ syntax
*)
#use "topfind"
#require "unix"
#require "str"
#use "General.ml"
#use "Version.ml"

type change={
 description:string;
 author:string;
 date:string
}

let rec process=
 let at=Str.regexp_string "@" in
 function
  | dt::ttl::r ->
     let change=Scanf.sscanf dt
      "%_s %s %i %_i:%_i:%_i CEST %i  %s"
      begin
       fun month day year email ->
        {
         description=String.chopStart ttl 4;
         date=Printf.sprintf "%s %i %i" month day year;
         author=List.hd (Str.split at email)
        }
      end
     in
     change::(skipBlank r)
  | _ -> failwith "Parse error 1."
and skipBlank=function
 | ""::l -> process l
 | [] -> []
 | _ -> failwith "Parse error 2."

let () =
 let changes=
  let lines,_=with_open_process_full
   "darcs changes"
   (fun (ic,_,_) ->channelToStringList ic)
  in
  process lines
 and lastDate = ref ""
 in
 Printf.printf "====Changelog====
This changelog was automatically generated from the darcs repository";
 List.iter
  begin
   fun c ->
    if c.date <> !lastDate then begin
     Printf.printf "==%s==\n" c.date;
     lastDate:=c.date
    end;
    if c.author<>author then
     Printf.printf " - %s (**%s**)\n" c.description c.author
    else
     Printf.printf " - %s\n" c.description
  end
  changes
