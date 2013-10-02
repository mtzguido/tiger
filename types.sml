structure types =
struct

type uniq = unit ref
datatype TigerType = TUnit
                   | TNil
                   | TInt
                   | TIntRO
                   | TString
                   | TRecord of (string * TigerType) list * uniq
                   | TArray of TigerType * uniq
                   | TReference of TigerType option ref
end
