
#Name: plot_images()
#Objective: plot images of processed data to the working folder
#Requirements: processed data including correlation matrix, network and SUVr data depending on the plots requested
#Outside variables needed: 
#Output: TIFF images in the working folder containing matrix, network and SUVr plots/charts
#To do: Check which plotting implementation is better for each of network, corr matrix and charts
#       so far you used igraph for network, ggplot2 for charts and corrplot for corr matrix

plot_images <- function(plot_analysis="full", group_ntw_by="Region", order_met, order_ref, order_modif, timepoints) {
    
    #Colours
    colours_pal<-   colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                              "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), bias= 0.2)
   
    # SUVr charts 
    pt <- ggplot(data = tidy_data)+
        
        geom_line( aes( x=Time, y=SUV, group=Animal), alpha=0.3, color="red", size=0.33) +
        ylab("{SUVr}g/mL") +xlab("Time (months)")+
        facet_wrap(~Area, ncol=6)+
        scale_color_brewer(palette = "Set3")+
        theme_bw()+ 
        geom_hline(yintercept = 1, linetype="dashed", alpha=0.6, size=0.4)+
        ggtitle("Metabolism x Time for each ROI")+
        
        theme(strip.background = element_rect(colour = "black", fill="white"),
              strip.text = element_text(colour = "black"), axis.title = element_text(size=17),
              axis.title.y.left= element_text(vjust =3.8),
              plot.margin = margin(1,0.6,0.6,1, "cm"),
              plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)
        )+
        scale_x_continuous(breaks = as.numeric(timepoints))+
        scale_y_continuous(expand = expand_scale(0))+
        geom_smooth(aes(x=Time, y=SUV), size=0.75, se=F)
    
    
    tiff("SUVr.tiff", width = 9, height = 9, units="in", res = 400, antialias = "cleartype")
    print(pt)
    dev.off()
    
    # Matrix correlogram
    if(order_modif) {corr_ord <- corrMatOrder(r[[order_ref]], order = order_met)
    }
    tiff("all_corrs.tiff", width = 12, height = 9, units="in", res = 500, antialias = "cleartype")
    par(mfrow=c(2,3))
    for (i in 1:length(r)) {

        if(order_modif) {r[[i]] <-   r[[i]][corr_ord,corr_ord]
        }
        corrplot( r[[i]], method = "shade", p.mat = NULL, insig = "blank",
                  cl.length = 11 , cl.cex = 0.8, cl.lim = c(0,1) ,
                  mar= c(2,0,0,0), addgrid.col = NA,
                  #title = paste(timepoints[i], "Months of Age",sep=" "),
                  tl.cex = 0.5, tl.col = "black"  ,  tl.offset = 0.2, 
                  col= colours_pal(sum(r[[i]]!=0&r[[i]]!=1)/2)
        )
    }
    dev.off()
    
    # Network graph
    tiff("all_netw.tiff", width = 18, height = 12, units = "in" ,res = 400, type = "cairo")
    par(mfrow=c(2,3))
    
    for (i in 1:length(N)) {
        
        # Extracting the same color for edges as in the correlation matrix     
        assign.color <- function(dat =E(N[[i]])$weight, color =colours_pal(sum(r[[i]]!=0&r[[i]]!=1)/2)) {
            newcorr <- (dat + 1)/2
            newcorr[newcorr <= 0] <- 0
            newcorr[newcorr >= 1] <- 1 - 1e-16
            color[floor(newcorr * length(color)) + 1]
        } 
        E(N[[i]])$color <- assign.color()
        
        plot.igraph(N[[i]],  vertex.size=7 , vertex.shape="circle", vertex.label.color="black",
                    vertex.color=as.numeric(as.factor(vertex_attr(N[[i]], group_ntw_by))),
                    vertex.label.cex=0.7, vertex.frame.color="gray",
                    edge.curved=.2, edge.width=0.3 ,
                    main=paste(timepoints[i], "Months of Age",sep=" "),
                    layout=layout_in_circle)
       
        
    }
    
    dev.off()
    
    
    #plot threshold variation chart
    
    pt <- ggplot(data = tidy_zeros)+
       
        geom_line( aes( x=Threshold_v, y=n_zeros, group=Time_point, color=Time_point), alpha=1 , size=0.6) +
        ylab("Number of zeros") +xlab("Threshold value")+
        scale_color_brewer(palette = "Set3")+
        theme_bw()+ 
        ggtitle("Number of zeros x Threshold")+
        
        theme(strip.background = element_rect(colour = "black", fill="white"),
              strip.text = element_text(colour = "black"), axis.title = element_text(size=19),
              axis.title.y.left= element_text(vjust =5),
              plot.margin = margin(2,0.3,0,3.8, "cm"),
              plot.title = element_text(size = 28, hjust = 0, vjust = 4)
        )+
        scale_x_continuous(breaks = seq(0,1,0.1))+
        geom_smooth(aes(x=Threshold_v, y=n_zeros), size=0.6)
    
    
    tiff("zeros_values.tiff", width = 12, height = 9, units="in", res = 300, antialias = "cleartype")
    print(pt)
    dev.off()
   
    # Graph metrics charts
    
    pt <- ggplot(data = G_attr)+
        
        geom_line( aes( x=Time, y=values), alpha=0.6, color="red", size=1.2) +
        xlab("Time (months)")+
        facet_wrap(~metric, scales = "free")+
        theme_bw()+ 
        ggtitle("Metric value over time")+
        
        theme(strip.background = element_rect(colour = "black", fill="white"),
              strip.text = element_text(colour = "black"), axis.title = element_text(size=17),
              axis.title.y.left= element_text(vjust =3.8),
              plot.margin = margin(2,0.3,0,3.8, "cm"),
              plot.title = element_text(size = 24, hjust = 0.5, vjust = 3)
        )+
        scale_x_continuous(breaks = as.numeric(timepoints))+
        geom_point(aes(x=Time, y=values), size=1)
        
    
    tiff("Ntw_metrics.tiff", width = 12, height = 9, units="in", res = 300, antialias = "cleartype")
    print(pt)
    dev.off()
    
    
    # Nodal metrics charts 
    pt <- ggplot(data = V_attr, aes(fill=Region, color=Region))+
        
        geom_line(aes( x=Time, y=values, group=name), alpha=0.55, size=0.25) +
        xlab("Time (months)")+
        facet_wrap(~metric, scales = "free")+
        theme_bw()+ 
        ggtitle("Graph Metric over time for each ROI")+
        
        theme(strip.background = element_rect(colour = "black", fill="white"),
              strip.text = element_text(colour = "black"), axis.title = element_text(size=17),
              axis.title.y.left= element_text(vjust =3.8),
              plot.margin = margin(2,0.3,0,3.8, "cm"),
              plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)
        )+
        scale_x_continuous(breaks = as.numeric(timepoints))+
        geom_smooth(aes(x=Time, y=values, group=Region), size=1.2, alpha=0.2)+
        
        geom_line  (data=V_attr[V_attr$name=="HpcAD L"|V_attr$name=="HpcAD R"|V_attr$name=="HpcP L"|V_attr$name=="HpcP R",],
                  aes( x=Time, y=values, group=name), alpha=0.8, size=0.6, colour="yellow")
        
       
    tiff("Nodal_metrics.tiff", width = 12, height = 9, units="in", res = 300, antialias = "cleartype")
    print(pt)
    dev.off()
    
    remove("plot_images", pos=1)
}