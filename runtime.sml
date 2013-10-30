structure runtime = 
struct
    open types
    val func_list = 
    [("chr",       {formals=[TInt], ret=TString, extern=true, label="_tiger_chr", level=0}),
     ("concat",    {formals=[TString,TString], ret=TString, extern=true, label="_tiger_concat", level=0}),
     ("exit",      {formals=[TInt], ret=TUnit, extern=true, label="_tiger_exit", level=0}),
     ("flush",     {formals=[], ret=TUnit, extern=true, label="_tiger_flush", level=0}),
     ("getchar",   {formals=[], ret=TString, extern=true, label="_tiger_getchar", level=0}),
     ("not",       {formals=[TInt], ret=TInt, extern=true, label="_tiger_not", level=0}),
     ("ord",       {formals=[TString], ret=TInt, extern=true, label="_tiger_ord", level=0}),
     ("print",     {formals=[TString], ret=TUnit, extern=true, label="_tiger_print", level=0}),
     ("print_err", {formals=[TString], ret=TUnit, extern=true, label="_tiger_print_err", level=0}),
     ("print_int", {formals=[TInt], ret=TUnit, extern=true, label="_tiger_print_int", level=0}),
     ("size",      {formals=[TString], ret=TInt, extern=true, label="_tiger_size", level=0}),
     ("strcmp",    {formals=[TString,TString], ret=TInt, extern=true, label="_tiger_strcmp", level=0}),
     ("streq",     {formals=[TString,TString], ret=TInt, extern=true, label="_tiger_streq", level=0}),
     ("substring", {formals=[TString,TInt,TInt], ret=TString, extern=true, label="_tiger_substring", level=0})
    ]
end
