signature allocator =
sig
    (*
     * Given a function body (with temp registers) and frame,
     * allocate and modify the body to use only real registers.
     * (Easier said than done!
     *)
    val run : frame.Frame -> asm.instr list -> asm.instr list
end
