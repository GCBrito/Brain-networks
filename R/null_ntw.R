
#Name: null_ntw()
#Objective: Create multiple null hypothesis networks and saves graph metrics for them
#Requirements: Original experiment data or randomised data 
#Outside variables needed: 
#Output: 

null_ntw <- function( ) {
    
    V_attr_perm <- NULL
    G_attr_perm <- NULL
    
    for (i in 1:length(perm_data)) {
        
        a <- graph.adjacency(perm_data[[i]], mode = "undirected", diag = FALSE, weighted =TRUE)
        
            # Calculate node metrics here 
            
            
            V(a)$degree             <- centr_degree(a, loops = F)$res
            V(a)$strength           <- strength(a, loops = F)
            V(a)$page_rank          <- page_rank(a, directed = FALSE)$vector
            V(a)$eigen_centr        <- eigen_centrality(a, directed = FALSE)$vector 
            V(a)$betweenness        <- centr_betw(a, directed = FALSE)$res
            V(a)$closeness          <- centr_clo(a)$res   
            V(a)$subgraph           <- subgraph.centrality(a)/max(subgraph.centrality(a))
            V(a)$coreness           <- coreness(a) # k-core decomposition - highest means more central
            V(a)$transit            <- transitivity(a, type = "weighted")
            # Calculate network metrics here
            
            a <- set_graph_attr(a, "network_density",   edge_density(a, loops = FALSE)                    )
            a <- set_graph_attr(a, "degree_centr" ,     centr_degree(a, loops = FALSE)$centralization     )
            a <- set_graph_attr(a, "betweenness_centr", centr_betw(a, directed = FALSE)$centralization    )
            a <- set_graph_attr(a, "closeness_centr"  , centr_clo(a)$centralization                       )
            a <- set_graph_attr(a, "eigenvalue_centr" , eigen_centrality(a, directed = FALSE)$value       )
            a <- set_graph_attr(a, "degree_assort",     assortativity_degree(a, directed = FALSE)         )
            a <- set_graph_attr(a, "str_assort"  , assortativity(a,vertex.attributes(a)$strength ,  directed = F))
            a <- set_graph_attr(a, "betw_assort",  assortativity(a,vertex.attributes(a)$betweenness,directed = F))
            a <- set_graph_attr(a, "clos_assort",  assortativity(a,vertex.attributes(a)$closeness  ,directed = F))
            a <- set_graph_attr(a, "eigen_assort", assortativity(a,vertex.attributes(a)$eigen_centr,directed = F))
            a <- set_graph_attr(a, "clust_coeff",  transitivity (a ))
            
            # Calculate edge metrics here

          V_attr_perm[[i]] <- as_tibble(vertex.attributes(a))
          G_attr_perm[[i]] <- as_tibble(graph_attr(a))
          
    }
    assign("G_attr_perm", bind_rows(G_attr_perm), pos=1)
    assign("V_attr_perm", bind_rows(V_attr_perm), pos=1)

}