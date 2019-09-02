
#Name:
#Objective: 
#Requirements:
#Outside variables needed:
#Output:

#####chart plotting #####

plot_fdg <- function( ) {
  tdata <- NULL 
  for (i in 1:length(k)) {
    
  tmp <- as_tibble(k[[i]])
  colnames(tmp) <- af
  
  tmp2<- sapply(1:nrow(k[[i]]), paste, "a", sep="")
  
  tmp<-mutate(tmp, Animal=tmp2)
  
  tmp<-gather(tmp, 1:ncol(k[[i]]), key = "timep", value = "fdg" )
  storage.mode(tmp$timep) <- "numeric"
  
  tdata[[i]]<- mutate(tmp, Area=names(k[i])) %>% select(Area,1:3) 
  
  }
  tdata <- bind_rows(tdata)
  tdata <- inner_join(tdata, nois[-1], by= c("Area"="Short name"))
  tdata <- arrange(tdata,Region, Area, timep, Animal)
  assign("tdata", tdata, pos=1)
###########
  leves <- levels(as.factor(c(tdata$Region, tdata$Laterality)))
  for (i in as.list(leves)) {
    
  tmpdata <- filter(tdata, Region==i | Laterality==i ) 
  
  nms  <- paste(i, "fdg.tiff", sep = "_")
  nms2 <- paste("Metabolism x Time for each ROI", i, sep = " - ")
  
  tiff(nms, width = 12, height = 9, units="in", res = 400, antialias = "cleartype")
  af <- as.numeric(af)
  pt <- ggplot(data = tmpdata)+
   
    geom_line( aes( x=timep, y=fdg, group=Animal), alpha=0.28, color="red", size=0.4) +
    ylab("{SUVbw}g/mL") +xlab("Time (months)")+
    facet_wrap(~Area)+
    scale_color_brewer(palette = "Set3")+
    theme_bw()+ 
    ggtitle(nms2)+
    
    theme(strip.background = element_rect(colour = "black", fill="white"),
          strip.text = element_text(colour = "black"), axis.title = element_text(size=17),
          axis.title.y.left= element_text(vjust =3.8),
          plot.margin = margin(2,0.3,0,3.8, "cm"),
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)
          )+
    scale_x_continuous(breaks = af)+
    geom_smooth(aes(x=timep, y=fdg), size=0.75)
  
    
print(pt)
  dev.off()
  }
  rm(pt, pos = 1)
  rm("plot_fdg", pos=1)
  

} 
  