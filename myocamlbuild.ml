(*w
 * This is the ocamlbuild configuration file. It is based on a contribution by
 * Nicolas Pouillard.
 *
 * TODO:
 * Change it to depend only on pure ocaml, not on sed.
 *
 *)

open Ocamlbuild_plugin;;
open Command;;
open Ocamlbuild_pack;;

dispatch begin function
(*
  | Before_options ->
    (*Ocamlfind integration*)
    Options.ocamlc := (Sh"ocamlfind c");
    Options.ocamlopt := (Sh"ocamlfind opt");
    Options.ocamldep := (Sh"ocamlfind dep");
*)
 | After_options ->
    (*This could (should?) go in before_options but there's a bug in
      ocamlbuild, it is fixed in CVS version for OCaml 3.11*)
    (*Options.ocaml_lflags := ["-linkpkg"] @ !Options.ocaml_lflags;*)
    (*Ocamlbuild has a nasty bug: it won't create the empty included
      directories.
      This of course results in tricky errors...*)
    ListLabels.iter (!Options.include_dirs)
     ~f:(fun d ->
          let dir=(Filename.concat !Options.build_dir d) in
          Shell.mkdir_p dir)
 | After_rules ->
    dep  ["ocaml"; "ocamldep"; "syntax:pa_extRecTypes"]
         ["src/pa_extRecTypes.cma"];
    flag ["ocaml"; "pp"; "syntax:pa_extRecTypes"]
         (A"src/pa_extRecTypes.cma");
    rule "parse -> mly"
      ~dep:"%.parse"
      ~prod:"%.mly"
      begin fun env _ ->
        let parse = env "%.parse" and mly = env "%.mly" in
        Seq [
         (*Error reporting in ocamlyacc*)
         Cmd(S[Sh"echo"; A("# 1 \""^parse^"\""); Sh">"; Px mly]);
         Cmd(S[A"sed"; A"-e";
               A("s/\\([a-z].*\\):$/\\1:|a_\\1  \
                 { ParseInfo.setCurrentRule \"\\1\"; $1 }a_\\1:/");
               P parse; Sh">>"; Px mly]);
        ]
      end;
 | _ -> ()
end;;
