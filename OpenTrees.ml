(**
   This an camlp4 extension. This is usefull to define extensible reccursive
   types. Extension is done both through (hidden) open reccursion and
   polymorphic variant types.

   This is done behind the hood by using open reccursion and constraints.
*)
open Camlp4.PreCast
open Syntax
open Ast

let openGramModule = "Gram"

(**
   Generates a fresh id..
 *)
let fresh=
 let cpt=ref 0
 and _loc=Loc.ghost in
 fun () ->
  incr cpt;
  <:ident< $lid:Printf.sprintf "id_%i" !cpt$ >>

type gramItem= string * branch list
and branch =
 | Alias of rule_item
 | Variant of string * rule_item list
 | Super
and rule_item =
 | Option of rule_item
 | Atom of string
 | Tup of rule_item list
 | List of rule_item

let collect_rules r= List.map fst r

let odecl _loc constr s t =
 Ast.TyDcl (_loc, s , [ <:ctyp< 'cst >> ],t, constr)

let declsFlatten _loc f l =
 List.fold_left begin fun acc s ->
  <:ctyp< $acc$ and $f s$ >>
 end <:ctyp< >> l

(**
   Transforms an expression list to a tupple of the given expressions
*)
let exList2ExCom _loc= function
 | [] -> <:expr< >>
 | [e] -> e
 | l ->
    let r=List.fold_left begin fun tup t ->
     Ast.ExCom (_loc,tup,t)
    end <:expr< >> l in
    ExTup(_loc,r)

(**
*)
let idList2Pattern _loc = function
 | [] -> <:patt< >>
 | [i] -> <:patt< $id:i$ >>
 | l ->
    let r =List.fold_left begin fun tup i ->
     (*<:patt< $acc$,$id:i$ >>*)
     Ast.PaCom (_loc,tup,<:patt< $id:i$ >>)
    end <:patt< >> l in
    Ast.PaTup(_loc,r)


let import _loc super s =
 odecl _loc [] s <:ctyp< 'cst $id:super$.$lid:s$ >>

let rec ruleItem _loc known_rules = function
 | Option r -> <:ctyp< $ruleItem _loc known_rules r$ option >>
 | Tup l -> ruleItems _loc known_rules l
 | Atom s when List.mem s known_rules -> <:ctyp< '$lid:s$ >>
 | Atom s -> <:ctyp< $lid:s$ >>
 | List l -> <:ctyp< $ruleItem _loc known_rules l$ list >>

and ruleItems _loc known_rules=
 let convert=ruleItem _loc known_rules in
 function
  | [] -> <:ctyp< >>
  | [r] -> convert r
  | l ->
     let t=List.fold_left begin fun tup t ->
      Ast.TySta (_loc,tup,convert t)
     end <:ctyp< >> l in
     <:ctyp< $tup:t$ >>;;

let ruleBranch _loc knownRules super = function
 | Variant (s,rl) -> <:ctyp< `$s$ of $ruleItems _loc knownRules rl$ >>
 | Alias a -> ruleItem _loc knownRules a
 | Super ->
    match super with
     | None -> assert false
     | Some t -> t

let rule_body _loc knownRules super l=
 let isVariant= ref false in
 let body=
  List.fold_left begin fun acc s ->
   if not !isVariant then
    begin
     match s with
      | Variant _ -> isVariant := true
      | _ -> ()
    end;
   let br=ruleBranch _loc knownRules super s in
    <:ctyp< $acc$ | $br$ >>
  end <:ctyp< >> l
 in
 if !isVariant then
  <:ctyp< [ $body$ ] >>
 else
  body

(**
   Eliminates duplicate rules...
*)
let uniq r=
 let module E = struct
  include Map.Make(String)
  let add x y m=
    add x y (remove x m)
 end
 in
 let m=List.fold_left (fun m (s,l) -> E.add s l m) E.empty r
 in
 E.fold (fun s l rul -> (s,l)::rul) m []


(**
   Generates the actual ocaml type.
*)
let openRules _loc super it=
 let known_rules = collect_rules it in
 let constr =
  let cst =
   List.fold_left begin fun object_type typ ->
    <:ctyp< $lid:typ$ : '$lid:typ$ ; $object_type$>>
   end <:ctyp< >> known_rules
  in [ <:ctyp< 'cst >>, <:ctyp< < $cst$ ;.. > >> ]
 in
 let rule s l =
  let super = (
   match super with
    | Some i -> Some <:ctyp< 'cst $id:i$.$lid:s$ >>
    | None -> None
  ) in
  odecl _loc constr s (rule_body _loc known_rules super l)
 in
 let res=List.fold_left begin fun acc (s,l) ->
  <:ctyp< $acc$ and $rule s l$ >>
 end <:ctyp< >> it in
<:str_item< type $res$ >>
;;


let rec genIt _loc knownRules=
 let self s =genIt _loc knownRules s
 and genCom = genCom _loc knownRules in
 function
  | Atom s when List.mem s knownRules -> <:expr< T.$lid:s$ >>
  | Atom s -> <:expr< return >>
  | List it -> <:expr< mmap $self it$>>
  | Tup l ->
     let pat,process = genCom (fun i -> <:expr< $i$ >>) l
     in <:expr< fun $pat$ -> $process$ >>
  | Option it ->
     let pat,process = genCom (fun i -> <:expr< Some $i$ >>) [it]in
     <:expr<function
      | None -> return None
      | Some $pat$ -> $process$
         >>

and genCom _loc knownRules genRet l =
     let elems = List.map (fun ty -> (fresh ()),ty ) l in
     let ids = List.map fst elems in
     let retEl = List.map (fun i -> <:expr< $id:i$ >>) ids in
     let process = genBinders _loc knownRules
      <:expr< return ($genRet (exList2ExCom _loc retEl)$) >> elems
     in
     idList2Pattern _loc ids,process

(**
   Generates all the intermediare definitions for a variable
*)
and genBinders _loc knownRules expr =
 let self l = genBinders _loc knownRules expr l in
 function
  | (id,Atom s)::l when not (List.mem s knownRules) ->
     self l
  | (id,ri)::l ->
     <:expr< Mon.bind ($genIt _loc knownRules ri$ $id:id$) ( fun $id:id$ -> $self l$ )  >>
  | [] -> expr

and genRule _loc knownRules name= function
 | Variant (s,[]) -> <:match_case< `$s$ -> return `$s$ >>
 | Variant (s,rl) ->
    let pat,process=genCom _loc knownRules
     (fun i -> <:expr< `$s$ $i$>>)
     rl
    in
    let pat = Ast.PaApp (_loc,<:patt< `$s$ >>,pat) in
    <:match_case< $pat$ -> $process$ >>
 | Super ->
    begin
     <:match_case< #Old.$lid:name$ as i ->
     Mon.bind ($lid:name$ i) ( fun i -> return (i :> (o Gram.$lid:name$)))
     >>
    end
 | Alias _ -> assert false

and matchcaseFlatten _loc l =
 List.fold_left begin fun acc s ->
  <:match_case< $acc$ | $s$ >>
 end <:match_case< >> l

let genFunc _loc knownRules name=function
 | [] -> assert false
 | [Super] -> <:str_item< >>
 | [Alias it] -> <:str_item< let $lid:name$ = $genIt _loc knownRules it$ >>
 | l ->
    let cases = List.map (genRule _loc knownRules name) l in
    <:str_item< let $lid:name$ = function $matchcaseFlatten _loc cases$ >>

let genInOld _loc knownRules = function
  | None -> <:str_item< >>
  | Some super -> <:str_item< module Old = struct
       $List.fold_left
        begin
         fun acc i ->
          <:str_item<
        type $lid:i$ = i $id:super$.$lid:i$;;
        $acc$
        >>
        end <:str_item< >> knownRules$
    end >>

EXTEND Gram
 GLOBAL: str_item;

 str_item: LEVEL "top" [
  [ "gram"; e = extends ; "{" ; it=gram_items ; "}"
  ->
   let super =
    match e with
     | None -> None
     | Some i -> Some <:ident< $i$.$uid:openGramModule$ >>
   in
   let it=uniq it in(*This should be optional*)
   let trans s=
    Ast.TyDcl (_loc, s ,[],<:ctyp< cst $uid:openGramModule$.$lid:s$>> , [])
   and rules = collect_rules it in
   let closed_decls=declsFlatten _loc trans rules
    (*The loopback constraint...*)
   and cst =
    List.fold_left begin fun object_type typ ->
     <:ctyp< $lid:typ$ : $lid:typ$ ; $object_type$ >>
    end <:ctyp<>> rules
   in
   <:str_item<
    (*The open grammar*)
    module $uid:openGramModule$ = struct
     $openRules _loc super it$
    end;;
   type cst = < $cst$ >
   and $closed_decls$
   (*The mapper*)
   module Trav =
   struct
    module type AstDef=
    sig
     $List.fold_left
      begin
       fun acc i ->
        <:sig_item< type $lid:i$ ;; $acc$ >>
      end <:sig_item< >> rules
      $
    end;;
    module GetConstr(A:AstDef)=
    struct
     open A
     type t = < $cst$ >
    end;;
    module Conv(From:AstDef)(To:AstDef)(Mon:Monad.T)=
    struct
     (**Monadic functions...**)
     module MonHelp=Monad.Helper(Mon);;
     open MonHelp;;
     $match e with
      | None -> <:str_item< >>
      | Some i ->
          let conv = <:ident< $id:i$.Trav.Conv >> in
          <:str_item<module Super = $id:conv$(From)(To)(Mon) >>
          $;;

     (*The whole tree is translated from the first ast to the second*)
      module type Par =
      sig
       $List.fold_left
        begin
         fun acc i ->
          <:sig_item<
        val $i$ : From.$lid:i$ -> To.$lid:i$ m;;
        $acc$
        >>
        end <:sig_item< >> rules$
      end

      type i=GetConstr(From).t
      type o=GetConstr(To).t

      (*The whole tree is translated From -> To excepted the toplevel node*)
      module type T =
      sig
       open Mon
       open Gram
        $List.fold_left
        begin
         fun acc i -> <:sig_item<
        val $i$ : i $lid:i$ -> o $lid:i$ m;;
        $acc$>>
        end <:sig_item< >> rules$
      end

       $genInOld _loc rules super$;;

      module type Ext=functor (T:Par) -> T

      (*Performs a monadic traversal of the tree...*)
      module Base:Ext=
       functor(T:Par) ->
      struct
       $let st =match e with
        | None -> <:str_item< >>
        | Some i -> <:str_item<include Super.Base(T) >>
      in
      let rules=List.fold_left
       begin
        fun acc (name,l) -> <:str_item<
         $genFunc _loc rules name l$;;
       $acc$>>
       end <:str_item< >> it
      in
      <:str_item< $st$;; $rules$>>
       $
     end
    end

    (*Closes the loop usefull to define easilly transformation within the same ast*)
    module Map(Mon:Monad.T)=
    struct
     (*This grammar*)
     module Self=
     struct
      $let s =
       List.fold_left begin
       fun acc s -> <:str_item< $acc$ ;; type $lid:s^"_loop"$ = $lid:s$ >>
      end <:str_item< >> rules
       in
       List.fold_left begin
       fun acc s -> <:str_item< $acc$ ;; type $lid:s$ = $lid:s^"_loop"$ >>
      end s rules
       $
     end
     include Conv(Self)(Self)(Mon)
     module Make(E:Ext):T=
     struct
      module rec T:T=E(T)
      include T
     end
    end;;
 end;;
>>
]
 ];
 gram_items:[
  [it = LIST0 [ gram_item ] SEP ";" -> List.flatten it
  ]
 ];
 gram_item:[
  [ "import" ; l = LIST1 a_LIDENT SEP "," ->
     List.map  (fun s -> s,[Super]) l
  | id=a_LIDENT ; ":=" ; l = LIST1 rule_branch SEP "|" ->
     [id,l]
  | id=a_LIDENT ; ":=" ; r =  rule_item ->
     [id,[Alias r]]
  ]
 ];
 rule_item:[
  [id = a_LIDENT ->
    Atom id
  | "["; i = rule_item ;"]" ->
     List i
  | "["; l = LIST1 rule_item SEP "," ;"]" ->
     List (Tup l)
  | "("; l = LIST1 rule_item SEP "," ;")" ->
     Tup l
  |  i = rule_item ; "?" ->
     Option i
  ]
 ];
 rule_branch:[
  [ "`";id = a_UIDENT; l = LIST0 rule_item SEP "," -> Variant (id,l)
  | "super" -> Super
  ]
 ];
 uid:[
  [i = LIST1 a_UIDENT SEP "." ->
    let rec uid2ident = function
     | [a]-> <:ident<$uid:a$>>
     | a::b -> <:ident<$uid:a$.$uid2ident b$>>
     | [] -> assert false
    in
    uid2ident i
  ]
 ];

 (*grammar inheritence...*)
 extends:[
  ["extends"; i=uid  ->
    Some <:ident< $i$ >>
  | ->
     None
  ]
 ];

END
