expType <- c("Set-A"=1, "Set-C"=2, "Set-B"=3, "Simulation"=4)

mds.f <- mds.r <- greedyAvg.f <- greedyAvg.r <- greedyAvg11.f <- greedyAvg11.r <- c()
greedyAvg18.f <- greedyAvg18.r <- greedyAvg26.f <- greedyAvg26.r <- c()
pnsd.f <- pnsd.r <- c() 

expSize <- c()

type <- 1 #South Aisle
expSize[type] <- 2000 

pnsd.f[type] <- 2000 
pnsd.r[type] <- 1871

#forward
mds.f[type] <- 2000 #ratio, #ordinal:2000
greedyAvg.f[type] <- 1996
greedyAvg11.f[type] <- 83
greedyAvg18.f[type] <- 1777
greedyAvg26.f[type] <- 0
  
#reverse
mds.r[type] <- 5 #ratio&MeanError= 1 , #ordinal:0&MeanError= 1
greedyAvg.r[type] <- 171
greedyAvg11.r[type] <- 0
greedyAvg18.r[type] <- 1997
greedyAvg26.r[type] <- 0
  
  type <- 2   # South Window
  expSize[type] <- 2000
pnsd.r[type] <- 1201 
pnsd.f[type] <- 1348
  #forward
  mds.f[type] <- 0 #ratio& MeanError= 1.0055, #ordinal:0 &MeanError= 1.0055 #RefNode: 154 Success: 0 out of  2000 experiments
  greedyAvg.f[type] <- 1124
  greedyAvg11.f[type] <- 402
  greedyAvg18.f[type] <- 2
  greedyAvg26.f[type] <- 6
  
  #reverse
  mds.r[type] <- 1450  #ratio:1450 & MeanError= 1.003636, #ordinal:1454 #RefNode: 97 Success: 1450 out of  2000 experiments MeanError = 1
  greedyAvg.r[type] <- 883
  greedyAvg11.r[type] <- 1642
  greedyAvg18.r[type] <- 1
  greedyAvg26.r[type] <- 14
  
  type <- 3   # North Window
  expSize[type] <- 692
pnsd.f[type] <- 692 
pnsd.r[type] <- 692
  #forward
  mds.f[type] <- 692 #ratio, #ordinal: #RefNode: 140 Success: 692 out of  693 experiments MeanError= 0 (1. exp is corrupt)
  greedyAvg.f[type] <- 601
  greedyAvg11.f[type] <- 65
  greedyAvg18.f[type] <- 690
  greedyAvg26.f[type] <- 25
  
  #reverse
  mds.r[type] <- 692  #ratio, #ordinal: #RefNode: 152 Success: 692 out of  693 experiments MeanError= 1
  greedyAvg.r[type] <- 131
  greedyAvg11.r[type] <- 573
  greedyAvg18.r[type] <- 25
  greedyAvg26.r[type] <- 692
  
  type <- 4   # Simulation
  expSize[type] <- 2000
pnsd.f[type] <- 2000 
pnsd.r[type] <- 2000
#forward
mds.f[type] <- 1981 #ratio=1981, #ordinal: 1988 &MeanError= 1  #RefNode: 1 Success: 1981 out of  2000 experiments MeanError= 1
  greedyAvg.f[type] <- 2000
  greedyAvg11.f[type] <- 775
  greedyAvg18.f[type] <- 750
  greedyAvg26.f[type] <- 758
  
  #reverse
  mds.r[type] <- 1974  #ratio:1974, #ordinal:1989 #RefNode: 10 Success: 1989 out of  2000 experiments MeanError= 1 
  greedyAvg.r[type] <- 2000
  greedyAvg11.r[type] <- 637
  greedyAvg18.r[type] <- 673
  greedyAvg26.r[type] <- 639
  
plotSuccess <- function (eType, saveToFile=FALSE)
  {
    library(ggplot2)
    library(RColorBrewer)
    library(reshape)
    
    plotName <- names(expType[eType])
    dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy AvgCh26");
    results <- matrix(data = c(
      pnsd.f[eType],pnsd.r[eType],    # PNSD - Probabilistic Node Sequence Discovery
      mds.f[eType],mds.r[eType],    # MDS
      greedyAvg.f[eType],greedyAvg.r[eType], # Greedy Avg
      greedyAvg11.f[eType], greedyAvg11.r[eType],    # Greedy AvgCh11    
      greedyAvg18.f[eType], greedyAvg18.r[eType], #Greedy AvgCh18
      greedyAvg26.f[eType],greedyAvg26.r[eType]         #Greedy AvgCh26
    ),
    ncol = 2, byrow = TRUE, 
    dimnames = list(dataNames,     # Rownames
                    c("LeftToRight", "RightToLeft"))                  # Colnames
    ) 
    
    # Convert to percents:
    results <- round((results/expSize[eType]) * 100,2)
    
    results <- melt(results)
    
    colnames(results)=c("algorithm","direction", "success")
    
    #reorder bars
    results <- within(results, algorithm <- factor(algorithm, levels = dataNames))
    
    theme_update(axis.title.x = element_text(size = 13, vjust = -0.25),
                 axis.title.y = element_text(size = 13, angle = 90, vjust = 0.25),
                 axis.text.x = element_text(size = 11, color="black"),
                 axis.text.y = element_text(size = 11, color="black"),
                 title = element_text(size=13),
                 panel.background = element_blank(),
                 panel.grid.major = element_line(colour = "grey90"),
                 legend.position = "bottom",
                 legend.box = "horizontal",
                 legend.key = element_blank(), 
                 legend.background = element_rect(fill="white", size=0.25),
                 legend.text = element_text(size=11))
    
    p <- ggplot (results, aes(x=direction, y=success, fill=algorithm)) + geom_bar(stat="identity", position="dodge")
    p<- p + geom_bar(stat="identity", position="dodge", colour="white", show_guide=FALSE) # to hide legend slashes
    p <- p + labs(x="Direction") + scale_y_continuous("Success [%]", limits=c(0,100))
    p <- p + scale_fill_manual(name="Algorithm", values = rev(brewer.pal(6,"YlGnBu")), guide=guide_legend(nrow=2,))
    p <- p + geom_text(data=results, aes(y=(success),label=paste(success,"%",sep="")),position=position_dodge(width=0.9), size=3, vjust=0)
    #p <- p + geom_text(data=results, aes(y=(50), label=algorithm),position=position_dodge(width=0.9), size=3.5, vjust=0, angle=90)
    p <- p + ggtitle(plotName)
    
    print(p)
    if (saveToFile)
    {
      cat(plotName, "saving...\n")
      ggsave(filename=paste("./plots/",plotName,"-2.pdf",sep=""), plot=p, width=6.86, height=5)
      
    }
  }
  
#for (s in 1:4) plotSuccess(s,TRUE)
#   type <- n   # South Window
#   expSize[type] <- 2000
#   pnsd.f[type] <- 1111 
#   pnsd.r[type] <- 1111
#   #forward
#   mds.f[type] <-  #ratio, #ordinal:
#   greedyAvg.f[type] <-
#   greedyAvg11.f[type] <-
#   greedyAvg18.f[type] <-
#   greedyAvg26.f[type] <-
#   
#   #reverse
#   mds.r[type] <-  #ratio, #ordinal:
#   greedyAvg.r[type] <-
#   greedyAvg11.r[type] <-
#   greedyAvg18.r[type] <-
#   greedyAvg26.r[type] <-
  