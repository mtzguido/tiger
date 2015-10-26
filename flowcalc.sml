structure flowcalc :> flowcalc =
struct
    open graph flow asm temp set

    type fstate = {
        lmap : (label -> node option) ref,
        def : (node -> temp list) ref,
        use : (node -> temp list) ref,
        ismove : (node -> bool) ref,
        jmp : (node -> label list) ref
    }

    (* Adds edges to the CFG *)
    fun flow g (st : fstate) = List.app (flow1 st) (tolist (nodes g))
    and flow1 st n =
        let val jmap = (!(#jmp st))
            val lmap = (!(#lmap st))
            fun add1 l = case lmap l of
                             (* FIXME: (maybe?)
                              * This can only happen on the last
                              * label, let's ignore it for now
                              *)
                             NONE => ()
                           | SOME n2 => mk_edge n n2
         in List.app add1 (jmap n) end

    fun all e = fn _ => e
    fun oplus m v e = fn x => if x = v then e else m x

    fun initstate () = { lmap = ref (all NONE),
                         def = ref (all []),
                         use = ref (all []),
                         ismove = ref (all false),
                         jmp = ref (all [])
                       }

    fun make_nodes' prev g (s : fstate) [] = s
      | make_nodes' prev g (s : fstate) (i::is) =
        let val n = newNode g
         in case prev of
                NONE => ()
              | SOME p => mk_edge p n;
            case i of
                LABEL info => (
                    (#lmap s) := oplus (!(#lmap s)) (#lab info) (SOME n);
                    make_nodes' (SOME n) g s is )
              | MOVE info => (
                  (#def s) := oplus (!(#def s)) n [#dst info];
                  (#use s) := oplus (!(#use s)) n [#src info];
                  (#ismove s) := oplus (!(#ismove s)) n true;
                  make_nodes' (SOME n) g s is )
              | OPER info =>  (
                  (#def s) := oplus (!(#def s)) n (#dst info);
                  (#use s) := oplus (!(#use s)) n (#src info);
                  (#jmp s) := oplus (!(#jmp s)) n (#jump info);
                  make_nodes' (case #jump info of [] => SOME n | l => NONE) g s is )
         end

    (*
     * Makes  a  CFG  with   only  edges  for  regular
     * instructions (non-jumps) and  creates a map for
     * labels to  nodes. Also calculates  "use", "def"
     * "ismove", and "jmp" for every node
     *)
    fun make_nodes g is = make_nodes' NONE g (initstate ()) is

    fun flowcalc is =
        let val g = newGraph ()
            val st = make_nodes g is
         in flow g st;
            FGRAPH { control = g,
                     def = !(#def st),
                     use = !(#use st),
                     ismove = !(#ismove st)
                   }
         end
end
