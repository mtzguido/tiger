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

    val RV : ir.IRexp

    val addString : string -> ir.IRexp
    val wrapFun : ir.IR -> Level -> ir.IR

    val trCall : bool -> Level -> Level -> string -> ir.IRexp list -> ir.IRexp
end
