
#Name: ntw()
#Objective: Create network object representative of brain network 
#Requirements: correlation matrix with our without a threshold 
#Outside variables needed: r (correlation matrix), ntw_metrics (logical whether metrics should be calculated)
#Output: N (network as igraph object, might cointain metrics as attributes if requested)

ntw <- function(ntw_metrics, commun, timepoints){
    
    N     <- NULL #igraph object
    V_attr<- NULL
    G_attr<- NULL
    
    for (i in 1:length(r)) {
        
        a <- graph.adjacency(r[[i]], mode = "undirected", diag = FALSE, weighted =TRUE)
        
        if(ntw_metrics==TRUE)   {
            
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
            a <- set_graph_attr(a, "clust_coeff",  transitivity(a))
            
        }
        # Communities and cluster calculations
        if(commun) {
        walk_clust      <- cluster_walktrap(a)
        infomap_clust   <- cluster_infomap(a)
        edge.betw_clust <- cluster_edge_betweenness(a)
        louvain_clust   <- cluster_louvain(a)
        }
        
        for (l in 5:ncol(tidy_data)) { #set vertex attributes based on data specifications
        for (j in levels(as.factor(tidy_data[[l]]))) {
            a <- set.vertex.attribute(a, colnames(tidy_data[l]),  V(a)[levels(as.factor(tidy_data$Area[tidy_data[[l]]==j]))],j)
        }}
        # Saves tidy calculated metrics to object in R
        temp1 <- timepoints[i]
        V_attr[[i]] <- as_tibble(vertex.attributes(a)) %>% mutate(Time=as.numeric(temp1))
        G_attr[[i]] <- as_tibble(graph_attr(a)) %>% mutate(Time=as.numeric(temp1))
        
        N[[i]] <- a
    }
    G_attr <- bind_rows(G_attr) %>% gather(key="metric", value ="values", -Time)
    V_attr <- bind_rows(V_attr) %>% gather(key="metric", value ="values", -c(name, Time, Region, Laterality))
    assign("G_attr", G_attr, pos=1) 
    assign("V_attr", V_attr, pos=1)
    assign("N", N, pos=1)
    rm("ntw", pos=1)
}