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

fun elem x [] = false
  | elem x (y::ys) = x = y orelse elem x ys

fun typeToString t = typeToString' [] t
and typeToString' ll t = case t of
    TUnit => "TUnit"
  | TNil => "TNil"
  | TInt => "TInt"
  | TIntRO => "TIntRO"
  | TString => "TString"
  | TRecord (l,_) =>
        let
            fun pr_fields [] = ""
              | pr_fields [(n,t)] = " n="^(typeToString' ll t)^" "
              | pr_fields ((n,t)::rest) =
                 " n="^(typeToString' ll t)^", "^(pr_fields rest)
        in
            "TRecord of {"^(pr_fields l)^"}"
        end
  | TArray (t, _) => "TArray of "^(typeToString' ll t)
  | TReference r =>
        if elem r ll
            then "<cicle_ref>"
            else typeToString' (r::ll) (valOf (!r))

end
