structure types =
struct

type uniq = unit ref
datatype TigerType = TUnit
                   | TNil
                   | TInt of access
                   | TString
                   | TRecord of (string * TigerType) list * uniq
                   | TArray of TigerType * uniq
(*                   | TFunc of TigerType list * TigerType AL PEDO? *)
                   | TSinom of string * (TigerType option ref)
and access = RO | RW

end
