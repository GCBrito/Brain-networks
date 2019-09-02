
#Name: perm_test()
#Objective: permutate data and returns distribution for chosen statistic
#Requirements:
#Outside variables needed:  
#Output: distribution for statistic 

perm_test <- function( perm_n=10000 , p.value=0.05) {
    corr_quantiles <- NULL
    random_corr <- NULL
    tidy_distr <- NULL
    
    for (j in 1:length(r)) {
    
        temp2 <- NULL
        temp3 <- NULL
        
        a <- xp[[j]]
        b <- r[[j]]
        p.value <- (p.value)/2
       
        for (i in 1:perm_n) {
            temp2[[i]] <- cor(a, a[sample(nrow(a), nrow(a),replace=F),])
        }
    
        temp3 <- as_tibble(apply(simplify2array(temp2), 1:2, as.vector))
        tidy_distr[[j]]  <- gather(temp3, key = edge_dist, value = corr_value)
        temp3 <- as.matrix(temp3)
        temp1 <- matrix(0,nrow=2,ncol=ncol(temp3))
        for(i in 1:ncol(temp3)){
            temp1[,i] <- as.vector(quantile(temp3[,i],probs=seq(p.value, 1-(p.value), (1-p.value)-p.value)))
        }
        colnames(temp1) <- colnames(temp3)
       
        for(i in 1:length(b)){
            c <- between(b[[i]], min(temp1[,i]),max(temp1[,i]))
            if(c) { b[[i]] <- 0 }
        }
        r[[j]] <- b
        corr_quantiles[[j]] <- temp1 
        random_corr[[j]] <- temp3
    }
    assign("r", r, pos=1)
    assign("corr_quantiles", corr_quantiles, pos=1)
    assign("random_corr", random_corr, pos=1)
    assign("tidy_distr", tidy_distr, pos=1)
    rm("perm_test", pos=1)
}