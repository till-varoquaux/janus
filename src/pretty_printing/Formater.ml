(*w
 * ====Text formating====
 * This is based on a file Copyright (c) 1999 Christian Lindig <lindig@ips.cs.tu-bs.de>.
 *
 * This version has been modified by till varoquaux (till.varoquaux@gmail.com) to
 * allow non printed fomatting instructions.
 * See the [[Formater.mli.html|interface]] for more details.
 *)

(* debuging makes groups visible *)
let debug   = false

(* Auxilliary functions *)
let strlen  = String.length

(* definitions *)
let nl      = "\n"

(* A type for the different kind of (user visible) groups *)
type gmode =
    | GFlat             (* hgrp *)
    | GBreak            (* vgrp *)
    | GFill             (* fgrp *)
    | GAuto             (* agrp *)

(*w users build (complex) documents of this type *)
type doc =
    | DocNil
    | DocCons           of doc * doc
    | DocText           of string
    | DocFormat         of string
    | DocNest           of int * doc
    | DocBreak          of string
    | DocGroup          of gmode * doc

(*w constructor functions for documents *)

let (^^) x y            = DocCons(x,y)
let empty               = DocNil
let text s              = DocText(s)
let formatInst s        = DocFormat(s)
let nest i x            = DocNest(i,x)
let break               = DocBreak(" ")
let breakWith s         = DocBreak(s)

let hgrp d              = DocGroup(GFlat, d)
let vgrp d              = DocGroup(GBreak,d)
let agrp d              = if   debug
                          then DocGroup(GAuto, text "[" ^^ d ^^ text "]")
                          else DocGroup(GAuto, d)
let fgrp d              = if   debug
                          then DocGroup(GFill, text "{" ^^ d ^^ text "}")
                          else DocGroup(GFill, d)

(*w
 * Formatting turns a complex ^^doc^^ document into a simpler ^^sdoc^^ document
 *)
type sdoc =
    | SNil
    | SText             of string * sdoc
    | SFormat           of string * sdoc
    | SLine             of int    * sdoc    (* newline + spaces *)

(*w
 * ^^sdocToString^^ formats a simple document into a string: ^^SLIne^^ line
 * breaks are expanded into a newline followed by spaces
 *)
let rec sdocToString esc = function
    | SNil              -> ""
    | SText(s,d)        -> (esc s) ^ sdocToString esc d
    | SFormat(s,d)      -> s ^ sdocToString esc d
    | SLine(i,d)        ->
       let prefix = String.make i ' '
       in  (esc (nl ^ prefix)) ^ sdocToString  esc d

(*w ^^sdocToFile oc doc^^ formats ^^doc^^ into output channel ^^oc^^ *)
let sdocToFile esc oc doc =
    let pstr = output_string oc in
    let rec loop = function
        | SNil          -> ()
        | SText(s,d)    -> pstr (esc s); loop d
        | SFormat(s,d)  -> pstr s; loop d
        | SLine(i,d)    -> let prefix = String.make i ' '
          in  pstr (esc nl);
          pstr (esc prefix);
          loop d
    in
        loop doc

(*w
 * ^^agrp^^s are turned into ^^Flat^^ or ^^Break^^ groups - so their are only 3
 * different modes internally.
 *)
type mode =
    | Flat
    | Break
    | Fill

(*w
 * ^^fits^^ checks whether a documents up to the next ^^break^^ fits into w
 * characters. All kind of groups are considered flat: their breaks count as
 * spaces. This means the ^^break^^ this function looks for must not be inside
 * sub-groups.
 *)
let rec fits w = function
    | _ when w < 0                   -> false
    | []                             -> true
    | (_,_,DocNil)              :: z -> fits w z
    | (i,m,DocCons(x,y))        :: z -> fits w ((i,m,x)::(i,m,y)::z)
    | (i,m,DocNest(j,x))        :: z -> fits w ((i+j,m,x)::z)
    | (_,_,DocText(s))          :: z -> fits (w - strlen s) z
    | (_,_,DocFormat _)         :: z -> fits w z
    | (_,Flat, DocBreak(s))     :: z -> fits (w - strlen s) z
    | (_,Fill, DocBreak(_))     :: _ -> true
    | (_,Break,DocBreak(_))     :: _ -> true
    | (i,_,DocGroup(_,x))       :: z -> fits w ((i,Flat,x)::z)

(*w
 * ^^format^^ does the actual pretty printing. It turns a ^^doc^^ document into
 * a simple ^^sdoc^^ document
 *)
let rec format w k = function
    | []                             -> SNil
    | (_,_,DocNil)              :: z -> format w k z
    | (i,m,DocCons(x,y))        :: z -> format w k ((i,m,x)::(i,m,y)::z)
    | (i,m,DocNest(j,x))        :: z -> format w k ((i+j,m,x)::z)
    | (_,_,DocText(s))          :: z -> SText(s ,format w (k + strlen s) z)
    | (_,_,DocFormat(s))        :: z -> SFormat(s ,format w k z)
    | (_,Flat, DocBreak(s))     :: z -> SText(s ,format w (k + strlen s) z)
    | (i,Fill, DocBreak(s))     :: z -> let l = strlen s in
                                            if   fits (w - k - l) z
                                            then SText(s, format w (k+l) z)
                                            else SLine(i, format w  i    z)
    | (i,Break,DocBreak _)      :: z -> SLine(i,format w i z)
    | (i,_,DocGroup(GFlat ,x))  :: z -> format w k ((i,Flat ,x)::z)
    | (i,_,DocGroup(GFill ,x))  :: z -> format w k ((i,Fill ,x)::z)
    | (i,_,DocGroup(GBreak,x))  :: z -> format w k ((i,Break,x)::z)
    | (i,_,DocGroup(GAuto, x))  :: z -> if fits (w-k) ((i,Flat,x)::z)
                                        then format w k ((i,Flat ,x)::z)
                                        else format w k ((i,Break,x)::z)


let ppToString ?(escapeFunction=fun x -> x) w doc =
 sdocToString  escapeFunction (format w 0 [0,Flat,agrp(doc)])

let ppToFile ?(escapeFunction=fun x -> x) oc w doc =
 sdocToFile escapeFunction oc (format w 0 [0,Flat,agrp(doc)])
