
#Name: matrices_corr()
#Objective: Correlation matrix analysis without plotting or thresholding
#Requirements: xlsx with FDG data for each ROIS, xlsx with area name, laterality and macroregion 
#Outside variables needed:  corr.met, z.analysis, xp (SUVr data) 
#Output: r, p, z (if z.analysis==TRUE)

matrices_corr <- function (p.corr.met, z.analysis) {
    tmp1 <- lapply(xp, corr.test, adjust=p.corr.met, ci=FALSE)
    tmp2 <- NULL
    tmp3 <- NULL
    
    for (i in 1:length(tmp1)) {
        tmp2[[i]] <- tmp1[[i]]$p
        tmp3[[i]] <- tmp1[[i]]$r
}

for (l in 1:length(tmp2)) {
for (i in 1:nrow(tmp2[[l]])) { for(j in 1:i) {tmp2[[l]][i,j] <- tmp2[[l]][j,i]} } 
}
names(tmp2) <- letters[1:length(tmp2)]
names(tmp3) <- letters[1:length(tmp3)]

if(z.analysis==TRUE) {
    assign("r", tmp3, pos=1)
    for (i in 1:length(tmp3)) {
        tmp3[[i]]  <- fisherz(tmp3[[i]])
        tmp3[[i]][is.infinite(tmp3[[i]])] <- 0
    }
    assign("z", tmp3, pos=1)
} else {
    assign("r", tmp3, pos=1)
    }
assign("p", tmp2, pos =1)

rm("matrices_corr", pos = 1)
}

