structure color :> color =
struct
    open graph

    datatype coloring =
        OK of (node -> int)
      | FAILED of node

    fun color k graph = FAILED (hd (nodes graph))
end
