
complete_ntw <- function(analysis="full", SUVr="Pons", z.analysis=FALSE, p.corr.met="none",
                         timepoints=c("5","8","11","14","17","20"), p.value=0.05,
                         order_met="hclust" , order_ref=1, order_modif=FALSE,
                         ntw_metrics=FALSE, commun=FALSE, threshold_chart=0:1,
                         threshold=0, neg_weights=FALSE, excel.export=FALSE) {

    source("preparing.R")
    preparing(SUVr=SUVr, timepoints=timepoints)

    if(analysis=="full")   {

        matrices_corr(p.corr.met=p.corr.met, z.analysis=z.analysis)
        threshold(threshold=threshold, neg_weights=neg_weights, threshold_chart=threshold_chart)
        perm_test()
        ntw(ntw_metrics=ntw_metrics, commun=commun,timepoints=timepoints)
        means_func(timepoints=timepoints)
        if (excel.export){exceldata()}
            rm("exceldata", pos=1)
        plot_images(order_met=order_met, order_ref=order_ref, order_modif=order_modif, timepoints=timepoints)

    }

    if(analysis=="matrix") {
        matrices_corr()

    }

    if(analysis=="network"){
        matrices_corr()

    }

    if(analysis=="charts") {


    }
    rm("complete_ntw", pos=1)

}
