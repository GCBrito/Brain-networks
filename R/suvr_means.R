# compute means and statistics for SUVr

suvr_means <- function() {

    mapply( function(x,y) { append(y, colMeans(x))
            },
        SUVr_data,list(NULL)
    ) %>%
        cbind(area=rownames(.)) %>%
            as_tibble() %>%
                gather("group", "mean_suvr",-area) %>%
                    assign("mean_suvr",.,pos=1)
}
