# compute means and statistics for SUVr

suvr_means <- function() {

    mapply( function(x,y) { append(y, colMeans(x))
            },
        SUVr_data,list(NULL)
    ) %>%
         %>%




}
