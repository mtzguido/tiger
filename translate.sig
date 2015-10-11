signature translate =
sig
    type Level
    type Access

    val outermost : Level
    val newLevel : {parent: Level, name: temp.label,
                    formals: bool list} -> Level

    val formals : Level -> Access list

    val allocLocal : Level -> bool -> Access

    val simpleVar : Access -> Level -> ir.IRexp
    val indexVar  : ir.IRexp -> ir.IRexp -> ir.IR

    val RV : ir.IRexp

    val trCall : bool -> Level -> Level -> string -> ir.IRexp list -> ir.IRexp

    val funcDecl : Level -> ir.IR -> unit
    val stringExp : string -> ir.IRexp
end
