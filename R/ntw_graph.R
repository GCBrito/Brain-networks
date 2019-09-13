# Graph creation

ntw_graph <- function() {

    lapply(corr_matrices, function(x) {
        graph.adjacency(x$r,mode = "undirected", diag = FALSE, weighted =TRUE)}
    ) %>%
        assign("ntw",., pos=1)
}
