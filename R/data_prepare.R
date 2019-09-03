#Import and manage data

data_prepare <- function(normalise="Pons", group_names=c("4","6","11","14","16","18")) {

    if (file.exists("subject_data.xlsx")) {
        v <- normalizePath("subject_data.xlsx")
    }   else{v <- file.choose()}

    specifications <- read_excel("data_specifications.xlsx")

    tmp1 <- NULL

    for (i in 1:length(excel_sheets(v))) {
        tmp1[[i]] <- read_excel(v, i)

        if (normalise=="Pons") {
            tmp1[[i]]  <-  tmp1[[i]]/as.matrix(tmp1[[i]][,"Pons"])
            tmp1[[i]]  <-  subset(tmp1[[i]], select=-c(Pons))
        }

        if (normalise=="whole") {
            tmp1[[i]] <-  tmp1[[i]]/rowMeans(tmp1[[i]])
            tmp1[[i]] <-  tmp1[[i]]
        }

        tmp1[[i]] <- tmp1[[i]][,match(specifications$Area , colnames(tmp1[[i]]))]
    }

    names(tmp1) <- excel_sheets(v)

    assign("SUVr_data", tmp1, pos=1)

    options(warn = -1)
    lapply(SUVr_data, as_tibble) %>%
        lapply(mutate, subject=paste("subject",1:nrow(SUVr_data$`1 passada`), sep="_")) %>%
            lapply(gather, Area, SUV, -subject) %>%
                mapply(cbind, ., Time=group_names, SIMPLIFY = F) %>%
                    bind_rows() %>%
                        inner_join(specifications[-1], by=c("Area"="Area")) %>%
                            arrange(Time, Region, Area, subject) %>%
                                as_tibble() %>%
                                    assign("tidy_data", ., pos=1)

    options(warn = 0)

    if (normalise!=FALSE) {
        paste ("Normalisation area used:", normalise, sep=" ") %>%
            noquote()
    }   else { ("Not normalised to any area") %>% noquote() }

}

