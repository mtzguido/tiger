structure escape :> escape = 
struct

open hash ast common 

fun S _ = ()

val nescapes = ref 0
val escape_names = ref []

fun found (s, info) = ( nescapes := !nescapes + 1;
                        escape_names := (s, info)::(!escape_names) )


fun marcarEscapes tree = 
let
    fun listar [] = ""
      | listar [(s,info)] =    s^" "^(info2str info)
      | listar ((s,info)::t) = s^" "^(info2str info)^", "^(listar t)
in
    ( nescapes := 0 ; 
     marcarEscapes' (tabNew ()) 0 tree ;
     if !verbose then print
      ("Escape: encontradas "
       ^(makestring (!nescapes))
       ^" variables escapadas.\n"
       ^(if !nescapes > 0
          then "Escape: son "^(listar (!escape_names))^".\n"
          else "")  )
     else ()
    )
end

and marcarEscapes' env d (UnitE _) = ()
  | marcarEscapes' env d (NilE _) = ()
  | marcarEscapes' env d (IntE _) = ()
  | marcarEscapes' env d (StringE _) = ()
  | marcarEscapes' env d (CallE ({args, ...}, _)) =
                  List.app (marcarEscapes' env d) args
  | marcarEscapes' env d (OpE ({left, right, ...}, _)) =
                              ( marcarEscapes' env d left ;
                                marcarEscapes' env d right )
  | marcarEscapes' env d (RecordE ({fields, ...},_)) =
                  List.app ((marcarEscapes' env d) o (fn r => #2 r)) fields
  | marcarEscapes' env d (SeqE (ll,_)) = List.app (marcarEscapes' env d) ll
  | marcarEscapes' env d (AssignE ({l,r}, info)) = ( marcarVar info env d l; marcarEscapes' env d r )
  | marcarEscapes' env d (IfE ({test,th,el= NONE}, _)) = ( marcarEscapes' env d test ;
                                                           marcarEscapes' env d th )
  | marcarEscapes' env d (IfE ({test,th,el= SOME e}, _)) = ( marcarEscapes' env d test ;
                                                             marcarEscapes' env d th ;
                                                             marcarEscapes' env d e )
  | marcarEscapes' env d (WhileE ({test,body},_)) = ( marcarEscapes' env d test ; marcarEscapes' env d body )
  | marcarEscapes' env d (ForE ({index, lo, hi, escape, body}, _)) = let val newenv = tabCopy env
                                                                         val _ = tabReplace newenv (index, (d,escape))
                                                                     in marcarEscapes' env d lo ;
                                                                        marcarEscapes' env d hi ;
                                                                        marcarEscapes' newenv d body  end
  | marcarEscapes' env d (LetE ({decs, body}, _)) = let val newenv = marcarDecls env d decs
                                                    in marcarEscapes' newenv d body end
  | marcarEscapes' env d (BreakE _) = ()
  | marcarEscapes' env d (ArrayE ({init, ...}, _)) = marcarEscapes' env d init
  | marcarEscapes' env d (VarE (var, info)) = marcarVar info env d var


and marcarVar info env d (SimpleVar s) = ( case tabFind env s of
                                        SOME (d', e) => if d > d' then (if !e then () else found (s, info) ; e := true ) else ()
                                        | NONE       => raise VarNoDec s
                                    ) (* hay que poner parentesis aca arriba... sneaky motherfu.. *)
  | marcarVar info env d (FieldVar (v,_)) = marcarVar info env d v
  | marcarVar info env d (IndexVar (v,e)) = ( marcarVar info env d v ; marcarEscapes' env d e )

and marcarDecls env d [] = env
  | marcarDecls env d (dec::decls) = let val newenv = marcarDecl env d dec
                                     in marcarDecls newenv d decls end

and marcarDecl env d (FuncDecl declist) =
            let fun proc1 ({params, body, ...},_) = 
                         let fun add_fld_to_tab (fld, tab) = (tabReplace tab (#name fld, ((d+1),(#escape fld))); tab)
                             fun combine flds tab = foldl add_fld_to_tab tab flds
                             val newenv = combine params (tabCopy env)
                         in marcarEscapes' newenv (d+1) body end
                in List.app proc1 declist ; env end

  | marcarDecl env d (VarDecl ({name, escape, typ, init}, _)) =
                                   ( marcarEscapes' env d init ;
                                     tabReplace env (name, (d, escape));
                                     env )
  | marcarDecl env d (TypeDecl _) = env

end (* struct *)
