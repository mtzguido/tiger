open hash
open ast

val esct : (symbol, int * bool ref) Tabla = tabNueva ()

val nescapes = ref 0

fun found () = nescapes := !nescapes + 1

fun marcarEscapes tree = ( nescapes := 0 ; 
                           marcarEscapes' (tabNueva ()) 0 tree ;
                           print ("Escape: encontradas "^(makestring (!nescapes))^" variables escapadas.\n") ;
                           ()
                          )

and marcarEscapes' env d (UnitE _) = ()
  | marcarEscapes' env d (NilE _) = ()
  | marcarEscapes' env d (IntE _) = ()
  | marcarEscapes' env d (StringE _) = ()
  | marcarEscapes' env d (CallE ({func, args}, _)) = List.app (marcarEscapes' env d) args
  | marcarEscapes' env d (OpE ({left, oper, right}, _)) = ( marcarEscapes' env d left ; marcarEscapes' env d right )
  | marcarEscapes' env d (RecordE ({fields,typ},_)) = List.app ((marcarEscapes' env d) o (fn r => #2 r)) fields
  | marcarEscapes' env d (SeqE (ll,_)) = List.app (marcarEscapes' env d) ll
  | marcarEscapes' env d (AssignE ({l,r}, _)) = ( marcarVar env d l; marcarEscapes' env d r )
  | marcarEscapes' env d (IfE ({test,th,el= NONE}, _)) = ( marcarEscapes' env d test ;
                                                           marcarEscapes' env d th )
  | marcarEscapes' env d (IfE ({test,th,el= SOME e}, _)) = ( marcarEscapes' env d test ;
                                                             marcarEscapes' env d th ;
                                                             marcarEscapes' env d e )
  | marcarEscapes' env d (WhileE ({test,body},_)) = ( marcarEscapes' env d test ; marcarEscapes' env d body )
  | marcarEscapes' env d (ForE ({index, lo, hi, escape, body}, _)) = let val newenv = tabRInserta (index, (d,escape), fromTab env)
                                                                     in marcarEscapes' env d lo ;
                                                                        marcarEscapes' env d hi ;
                                                                        marcarEscapes' newenv d body  end
  | marcarEscapes' env d (LetE ({decs, body}, _)) = let val newenv = marcarDecls env d decs
                                                    in marcarEscapes' newenv d body end
  | marcarEscapes' env d (BreakE _) = ()
  | marcarEscapes' env d (ArrayE ({typ, size, init}, _)) = marcarEscapes' env d init
  | marcarEscapes' env d (VarE (var, _)) = marcarVar env d var


and marcarVar env d (SimpleVar s) = ( case tabBusca (s, env) of
                                        SOME (d', e) => if d > d' then (if !e then () else found () ; e := true ) else ()
                                        | NONE       => raise Fail ("Escape: Variable "^s^" no definida.")
                                    ) (* hay que poner parentesisi aca arriba... sneaky motherfu.. *)
  | marcarVar env d (FieldVar (v,_)) = marcarVar env d v
  | marcarVar env d (IndexVar (v,e)) = ( marcarVar env d v ; marcarEscapes' env d e )

and marcarDecls env d [] = env
  | marcarDecls env d (dec::decls) = let val newenv = marcarDecl env d dec
                                     in marcarDecls newenv d decls end
(* and marcarDecls env d decs = foldr (fn dec => fn env' => marcarDecl env' d dec) env decs (* no anda... *)*)

and marcarDecl env d (FuncDecl declist) =
            let fun proc1 ({name, params, result, body},_) = 
                         let fun add_fld_to_tab (fld, tab) = tabRInserta((#name fld), ((d+1),(#escape fld)), tab)
                             fun combine flds tab = foldl add_fld_to_tab tab flds
                             val newenv = combine params env
                         in marcarEscapes' newenv (d+1) body end
                in List.app proc1 declist ; env end

  | marcarDecl env d (VarDecl ({name, escape, typ, init}, _)) =
                                   ( marcarEscapes' env d init ;
                                     tabRInserta (name, (d, escape), env) )
  | marcarDecl env d (TypeDecl _) = env
