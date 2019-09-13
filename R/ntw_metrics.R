# graph metrics

ntw_metrics <- function() {

    for (i in 1:length(ntw)) {
        vertex.attributes(ntw[[i]])$degree        <- centr_degree(ntw[[i]], loops = F)$res
        vertex.attributes(ntw[[i]])$strength      <- strength(ntw[[i]], loops = F)
        vertex.attributes(ntw[[i]])$v_clust_coeff <- transitivity(ntw[[i]], type = "weighted")

        ntw[[i]] <- set_graph_attr(ntw[[i]], "network_density", edge_density(ntw[[i]], loops = FALSE)  )
        ntw[[i]] <- set_graph_attr(ntw[[i]], "mean_strength",   mean(strength(ntw[[i]],loops = FALSE)))
        ntw[[i]] <- set_graph_attr(ntw[[i]], "clust_coeff",     transitivity(ntw[[i]])  )

    }
    ntw %>%
        mapply (function(x,y)  {
        vertex.attributes(x) %>% as_tibble() %>%
            mutate(group=y)
        } ,x=.,y=names(.), SIMPLIFY = F
    ) %>% bind_rows() %>%
        gather(key="metric", value ="values", -c(group,name)) %>%
            assign("v_attr", ., pos=1)

    ntw %>%
        mapply (function(x,y)  {
            graph.attributes(x) %>% as_tibble() %>%
                mutate(group=y)
        } ,x=.,y=names(.), SIMPLIFY = F
    ) %>% bind_rows() %>%
        gather(key="metric", value ="values", -c(group)) %>%
            assign("g_attr", ., pos=1)

}
