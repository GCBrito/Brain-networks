
#Name: preparing()
#Objective: load needed resources, manipulate data and creates NULL variables
#Requirements: all scripts in local folder, files with data to be analysed
#Outside variables needed: SUVr
#Output: "xp" with prepared data, "specifications" with data details, "tidy_data" for further analysis
#To do: revise last part, as might be bugged with different time series or groups

preparing <- function(SUVr, timepoints) {

    source("ntw.R")
    source("xlsx_data.R")
    source("matrices_corr.R")
    source("means_func.R")
    source("threshold.R")
    source("plot_images.R")
    source("perm_test.R")

    if(file.exists("subject_data.xlsx")){
                v <- normalizePath("subject_data.xlsx")
    } else {    v <- file.choose()
    }

    tmp2 <- read_xlsx("data_specifications.xlsx")
    assign("specifications", tmp2, pos=1)
    tmp1 <- NULL

    for (i in 1:length(excel_sheets(v))) {
        tmp1[[i]] <- as.matrix(read_xlsx(v, i))

        if (SUVr=="Pons") {
            tmp1[[i]]  <-  tmp1[[i]]/tmp1[[i]][,"Pons"]
            tmp1[[i]]  <-  subset(tmp1[[i]], select=-c(Pons))
        }

        if (SUVr=="whole") {
            tmp1[[i]] <-  tmp1[[i]]/rowMeans(tmp1[[i]])
            tmp1[[i]] <-  tmp1[[i]]
        }
        tmp1[[i]] <- tmp1[[i]][,match(specifications$Area , colnames(tmp1[[i]]))]
    }
    names(tmp1) <- letters[1:length(tmp1)]

    assign("xp", tmp1, pos=1)



    xns <- lapply(SUVr_data, as_tibble) %>% lapply(mutate, subject=paste("subject",1:nrow(SUVr_data$`1 passada`), sep="_")) %>% lapply(gather, Area, SUV, -subject)

    xns <- mapply(cbind, xns, Time=as.numeric(timepoints), SIMPLIFY = F)
    xxs <- bind_rows(xns)
    xxs <- xxs %>% inner_join(specifications[-1], by=c("Area"="Area"))
    xxs <- arrange(xxs, Time, Region, Area, Animal)
    assign("tidy_data", xxs, pos=1)





    rm("preparing", pos=1)
    }



