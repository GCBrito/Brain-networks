#Correlation matrix processing

corr_matrix <- function(adj.met="fdr" ) {

    mapply(corr.test, SUVr_data, y=SUVr_data, MoreArgs = list(adjust=adj.met), SIMPLIFY=F) %>%
        lapply( function(x) {
                    append (x, list(z=fisherz(x$r)), after=3 )
                }
        ) %>%
            assign("corr_matrices", .,pos=1)

}
