structure flowcalc :> flowcalc =
struct
    open graph flow

    fun flowcalc instrs =
        FGRAPH { control = newGraph (),
                 def = fn _ => [],
                 use = fn _ => [],
                 ismove = fn _ => false
               }
end
