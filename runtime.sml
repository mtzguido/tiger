structure runtime =
struct
    open types translate

    val func_list =
    [("chr",       {formals=[TInt], ret=TString, extern=true, label="_tiger_chr", level=outermost}),
     ("concat",    {formals=[TString,TString], ret=TString, extern=true, label="_tiger_concat", level=outermost}),
     ("exit",      {formals=[TInt], ret=TUnit, extern=true, label="_tiger_exit", level=outermost}),
     ("flush",     {formals=[], ret=TUnit, extern=true, label="_tiger_flush", level=outermost}),
     ("getchar",   {formals=[], ret=TString, extern=true, label="_tiger_getchar", level=outermost}),
     ("not",       {formals=[TInt], ret=TInt, extern=true, label="_tiger_not", level=outermost}),
     ("ord",       {formals=[TString], ret=TInt, extern=true, label="_tiger_ord", level=outermost}),
     ("print",     {formals=[TString], ret=TUnit, extern=true, label="_tiger_print", level=outermost}),
     ("print_err", {formals=[TString], ret=TUnit, extern=true, label="_tiger_print_err", level=outermost}),
     ("print_int", {formals=[TInt], ret=TUnit, extern=true, label="_tiger_print_int", level=outermost}),
     ("size",      {formals=[TString], ret=TInt, extern=true, label="_tiger_size", level=outermost}),
     ("strcmp",    {formals=[TString,TString], ret=TInt, extern=true, label="_tiger_strcmp", level=outermost}),
     ("streq",     {formals=[TString,TString], ret=TInt, extern=true, label="_tiger_streq", level=outermost}),
     ("substring", {formals=[TString,TInt,TInt], ret=TString, extern=true, label="_tiger_substring", level=outermost})
    ]
end
