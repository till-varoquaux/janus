open General
open Camlp4.PreCast
open Syntax
open Ast
open Camlp4Helper
open Grammar.Types


let rec genIt gram=
  let self s =genIt gram s
  and genCom = genCom gram in
  function
   | Atom s when Grammar.mem s gram -> <:expr< Self.$lid:s$ >>
   | Atom _ | Nativ _ -> <:expr< Mon.return >>
   | List it -> <:expr< MonHelp.mmap $self it$>>
   | Tup l ->
      let pat,process = genCom (fun i -> <:expr< $i$ >>) l
      in <:expr< fun $pat$ -> $process$ >>
   | Option it ->
      let pat,process = genCom (fun i -> <:expr< Some $i$ >>) [it]in
      <:expr<function
       | None -> Mon.return None
       | Some $pat$ -> $process$
          >>

and genCom gram genRet l =
  let elems = List.map l ~f:fun ty -> (fresh ()),ty in
  let idPats = List.map elems ~f:fun (i,_) -> <:patt< $id:i$ >> in
  let retEl = List.map elems ~f:fun (i,_) -> <:expr< $id:i$ >> in
  let process = genBinders gram
   <:expr< Mon.return ($genRet (exList2ExCom retEl)$) >> elems
  in
  pattList2Pattern idPats,process

 (*w
  *   Generates all the intermediare definitions for a variable
  *)
and genBinders gram expr =
  let self l = genBinders gram expr l in
  function
   | (_,Atom s)::l when not (Grammar.mem s gram) ->
      (*
       * This item is not part of the grammar, it doesn't need to be binded
       *)
      self l
   | (id,ri)::l ->
      <:expr< Mon.bind ($genIt gram ri$ $id:id$) ( fun $id:id$ -> $self l$ ) >>
   | [] -> expr

and genRule gram name= function
  | Other s -> <:match_case< #$lid:s$ as v -> Mon.return v >>
  | Labeled (s,[]) -> <:match_case< `$s$ -> Mon.return `$s$ >>
  | Labeled (s,rl) ->
     let pat,process=genCom gram
      (fun i -> <:expr< `$s$ $i$>>)
      rl
     in
     let pat = Ast.PaApp (_loc,<:patt< `$s$ >>,pat) in
     <:match_case< $pat$ -> $process$ >>
  | Super ->
     <:match_case< #Super.In.$lid:name$ as i ->
   Mon.bind ($lid:name$ i) ( fun i -> Mon.return (i :> (to_dict Gram.$lid:name$)))
   >>

and genVariant gram = function
  | s,[] -> <:match_case< $uid:s$ -> Mon.return $uid:s$ >>
  | s,rl ->
     let pat,process=genCom gram
      (fun i -> <:expr< $uid:s$ $i$>>)
      rl
     in
     let pat = Ast.PaApp (_loc,<:patt< $uid:s$ >>,pat) in
     <:match_case< $pat$ -> $process$ >>

let genFunc gram name=function
  | Abstract -> <:str_item< let $lid:name$ _ = assert false >>
  | Import -> <:str_item< >>
  | Alias it -> <:str_item< let $lid:name$ = $genIt gram it$ >>
  | Variant l ->
     let cases = List.map l ~f:(genVariant gram) in
     <:str_item< let $lid:name$ = function $matchcaseList2matchcase cases$
       >>
  | PolVar l ->
     let cases = List.map l ~f:(genRule gram name) in
     <:str_item< let $lid:name$ = function $matchcaseList2matchcase cases$
       >>

let gen gram=
  Grammar.fold gram
   ~init:<:str_item< >>
   ~f:(fun ~key:name ~data:l acc -> <:str_item<
        $genFunc gram name l$;;
       $acc$>>)
