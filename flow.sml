structure flow =
struct
    datatype flowgraph =
        FGRAPH of { control: graph.graph,
                    def: graph.node -> temp.temp list,
                    use: graph.node -> temp.temp list,
                    ismove: graph.node -> bool
                  }
end
