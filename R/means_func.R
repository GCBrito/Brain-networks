
#Name:
#Objective: 
#Requirements:
#Outside variables needed:
#Output:

### areas means ###

means_func <- function(timepoints) {

assign("af", timepoints, pos=1)  
m <- matrix(ncol = ncol(xp[[1]]), nrow = length(xp))
n <- NULL

for (j in 1:length(xp)) {

for (i in 1:ncol(xp[[j]])) {
  
n[[i]] <- mean(xp[[j]][,i])

}
m[j,] <- n
}

colnames(m) <- colnames(xp[[1]])

### 

k <- NULL 
bd <- do.call(rbind, xp)
for (i in 1:ncol(bd)) {

 k[[i]] <- matrix(bd[,i], ncol=length(xp), nrow=nrow(xp$a))
  
}
names(k) <- colnames(xp$a)


mn<- as_tibble(m)
mn <- mutate(mn, timepoint=timepoints) %>% gather( "Area", "fdg", 1:ncol(m) ) 
mn<- as.data.frame(mn)
storage.mode(mn$timepoint) <- "numeric" 

assign("ROI_SUVr", k, pos=1) # list containing a matrix of SUVr for each area along time-points
assign("SUVr_mean", m, pos=1) # mean SUVr for each time-point for each area
assign("tidy_meanSUVr", mn, pos=1) 
rm("means_func", pos = 1)
rm("af", pos=1)

}