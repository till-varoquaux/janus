(*w
 * ==== Text formatting  ====
 *
 * This is more or less taken verbatim from Christian Liding's Pp module. This
 * implementation was documented in his article "Strictly pretty".
 *
 * This module provides a pretty printer.  Documents are built from a set of
 * primitives which later can be formatted for different line length.  The
 * pretty printer is intended for tree like structures like source code.  It
 * does not provide a two dimensional layout like TeX for example.  This pretty
 * printer is heavily inspired by a pretty printer proposed by Philip Wadler in
 * his paper "A prettier printer" available from his home page and to appear in
 * the Journal of Functional Programming in 1999.
 *
 *)

type doc
 (*w
  * ^^doc^^ represents documents which can be pretty printed. They are
  * constructed from functions provided by this module.
  *)

val empty : doc
 (*w ^^empty^^ is the empty document. *)

val (^^) : doc -> doc -> doc
 (*w ^^d1 ^ ^ d2^^ is the concatenation of documents ^^d1^^ and ^^d2^^. *)

val text : string -> doc
 (*w
  * ^^text "hello"^^ creates a document containing the string ^^"hello"^^.
  * This operators simply turns strings into documents.
  *)


val formatInst : string -> doc
 (*w
  * ^^formatInst s^^ is used to put formatting informations. The text will be
  * printed but will not be used to place linebreaks.
  *)

val break : doc
 (*w
  * ^^break^^ represents a point where the pretty printer can decide whether to
  * begin a new line or not. In the former case a newline is printed, followed by a
  * number of spaces to indent the next line. The number of spaces is controlled
  * by the ^^nest^^ operator. When the pretty printer does not begin a new line a
  * single space is printed for the ^^break^^.
  *)

val breakWith : string -> doc
 (*w
  * ^^breakWith s^^ acts like ^^break^^ except that it is not turned into aspace or
  * a newline but into ^^s^^ or a newline (followed by spaces).
  *)

val nest : int -> doc -> doc
 (*w
  * ^^nest i doc^^: all breaks inside ^^doc^^ which are turned into newlines
  * will be followed by additional ^^i^^ spaces. Nesting adds up: when ^^doc^^
  * contains another ^^nest j^^ operator the breaks inside its document will
  * followed by ^^i+j^^ spaces.
  *)

val hgrp : doc -> doc
 (*w
  * ^^break^^s inside a ^^hgrp^^ (horizontal group) are never turned into
  * newlines, so they always come out as spaces.
  *)

val vgrp : doc -> doc
 (*w
  *^^break^^s inside a ^^vgrp^^ (vertical group) are always turned onto newlines
  * (which are followed by spaces as indicated by ^^nest^^).
  *)

val agrp : doc -> doc
 (*w
  * The automatic group ^^agrp^^ is the most versatile: when the whole group
  * including all subgroups fits into one line ^^break^^s come out as spaces.
  * Otherwise ^^break^^s come out as newlines.  However, this does not affect
  * subgroups:  their ^^break^^s are considered seperately.
  *)

val fgrp : doc -> doc
 (*w
  * The break policy inside an ^^agrp^^ is fixed for all breaks of the
  * group. Inside a flexible group ^^fgrp^^ each ^^break^^ is considered
  * individually: when the document up to the next ^^break^^ fits into the
  * current line the ^^break^^ comes out as space. Otherwise it comes out as
  * newline followed by spaces.
  *)

val ppToString : ?escapeFunction:(string -> string) -> int -> doc -> string
 (*w
  * ^^ppToString w doc^^ formats ^^doc^^ for line length ^^w^^ (> 0) and returns
  * it as string. This is not efficient and should be avoided for large documents.
  * The document is considered to be surrounded by a virtual ^^agrp^^ - if this
  * is not desired it should have itself a different group at its outermost level
  * to protect itself from the ^^agrp^^.
  *)

val ppToFile :
 ?escapeFunction:(string -> string) -> out_channel -> int -> doc -> unit
 (*w
  * ^^ppToFile oc w doc^^ pretty prints ^^doc^^ for line length ^^w^^ into an
  * output channel ^^oc^^. The document is considered to be surrounded by a
  * virtual ^^agrp^^ - if this is not desired it should have itself a different
  * group at its outermost level to protect itself from this ^^agrp^^.
  *)
