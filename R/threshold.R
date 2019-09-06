#Name: threshold()
#Objective: Applies an arbitrarily defined threshold to the correlation matrix for further analysis
#Requirements: A correlation matrix processed from SUVr data
#Outside objects needed: r, threshold
#Output: rt (correlation matrix with 0 in place of below-threshold correlations)
#To do:

threshold <- function(threshold, neg_weights, threshold_chart) {

    threshold_chart <- seq(min(threshold_chart), max(threshold_chart), 0.02)

    #Count how many zeroes below a list of thresholds (in threshold_chart argument)
    zeros<-NULL
    b <- NULL
    for (l in threshold_chart) {
        for (i in 1:length(r)) {
          b[[i]] <- (sum(abs(r[[i]])<l & r[[i]]!=1 ))/2
        }
      zeros[[as.character(l)]] <- b
    }
    zeros <- do.call(cbind, as.list(zeros))
    zeros <- as_tibble(zeros)
    zeros <-  mutate(zeros, "Time_point"=paste("Time_", 1:nrow(zeros) , sep="")) %>%
              gather(Threshold_v, n_zeros, 1:ncol(zeros))
    storage.mode(zeros$Threshold_v) <- "numeric"
    assign("tidy_zeros", zeros, pos=1)

    if(neg_weights==FALSE) {

      for (j in 1:length(r)) { for (i in 1:length(r[[j]])) { if ( r[[j]][[i]] < 0) { r[[j]][[i]] <- 0} }
      }

    }
    #Apply specific threshold for further analysis
    for (j in 1:length(r)) {
      for (i in 1:length(r[[j]])) { if ( abs(r[[j]][[i]]) < threshold) { r[[j]][[i]] <- 0}
      }
    }
    assign("r", r, pos = 1)
    rm(threshold, pos=1)
}
