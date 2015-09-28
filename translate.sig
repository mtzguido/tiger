signature translate =
sig
    type Level
    type Access

    val outermost : Level
    val newLevel : {parent: Level, name: temp.label,
                    formals: bool list} -> Level
    val allocLocal : Level -> bool -> Access

    val simpleVar : Access -> Level -> ir.IRexp

    val RV : ir.IRexp

    val addString : string -> ir.IRexp
end
