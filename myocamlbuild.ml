open Ocamlbuild_plugin;;
open Command;;
dispatch begin function
| After_rules ->
    dep  ["ocaml"; "ocamldep"; "syntax:OpenTrees"]
         ["OpenTrees.cmo"];
    flag ["ocaml"; "pp"; "syntax:OpenTrees"]
         (A"./OpenTrees.cmo");
    rule "parse -> mly"
      ~dep:"%.parse"
      ~prod:"%.mly"
      begin fun env _ ->
        let parse = env "%.parse" and mly = env "%.mly" in
        Cmd(S[A"sed"; A"-e";
              A("s/\\([a-z].*\\):$/\\1:|a_\\1  \
                 { ParseInfo.setCurrentRule \"\\1\"; $1 }a_\\1:/");
              P parse; Sh">"; Px mly])
      end;
| _ -> ()
end;;

