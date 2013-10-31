structure translate :> translate =
struct
    open ir frame common

    fun translate ir frame = 
        if !verbose
        then print ("generando código para: "^(frameName frame)^"\n")
        else ()
end
