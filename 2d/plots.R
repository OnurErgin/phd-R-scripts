library(ggplot2)

allResultsDF <- read.table("all_results.DF", header=TRUE)

######################### NOISE DIVERSITY ################################## 
plotNoiseDiversity <- function (allResultsDF, Nx, Ny, dx, dy, experimentSize, sd_noise_dbm, totalVerifiedSuccess) {
  noise_diversityDF <- subset(allResultsDF, Nx==5 & Ny==4 & dx==5 & dy==10 & experimentSize==100, select=c(5,8:19))
  
  
  # Plot total success
  p_success <- ggplot( noise_diversityDF, aes(x=sd_noise_dbm, y=totalVerifiedSuccess)) + geom_line(size=1.5) #geom_bar(stat="identity") #+ scale_x_reverse()
  p_success <- p_success + labs(x="Standard deviation of Noise Level [dbm]", y="Verified Success [%]") + 
                            scale_y_discrete(breaks=seq(from=0,to=100,by=10),limits=c(0:100)) + 
                            scale_x_continuous(breaks=seq(-80,-20,5)) +
                            theme(plot.title = element_text(lineheight=.8, face="bold", size=14)) + ggtitle("5x4 Grid, dx=5m, dy=10m")
  print(p_success)
  ggsave("SuccessXnoise_level.pdf")
}

# Plot noise distributions
plotNoiseDistributions <- function (noise_diversityDF) {
  Pn_collection <- data.frame()
  for (sd_noise_dbm in noise_diversityDF$sd_noise_dbm){
    sd_noise_mW <- dbm2mw(sd_noise_dbm)
    for (i in 1:100){
      Pn <- round(mw2dbm(rnorm(1, mean=0, sd=sd_noise_mW)^2))
      Pn_collection <- rbind(Pn_collection, data.frame(sd_noise_dbm=sd_noise_dbm, Pn=Pn))
    }
  }
  
  p_noise <- ggplot(Pn_collection, aes(x=sd_noise_dbm, y=Pn, group=sd_noise_dbm)) + geom_boxplot() #+ scale_x_reverse()
  p_noise <- p_noise + ylab("distribution") + xlab("Standard deviation of Noise Level [dbm]")
  print(p_noise)
}

#pdf(file=plotName, height=15, width=8)
#  grid.arrange(p_success,p_noise,ncol=1)
#dev.off()

########################## FOLDING PATH ################################# 
plotFoldingPath <- function (Truth, TruthAsSequence) {
  graph_input_truth <- melt(Truth, varnames=c("Y","X"))
  graph_input_truth <- graph_input_truth[match(TruthAsSequence, graph_input_truth$value),]
  map.truth <- ggplot(graph_input_truth, aes(x=X,y=Y, label=value)) +  
                      scale_y_reverse(breaks=as.integer(rownames(Truth)), limits=c(5,-2), name="Y [meters]") + # + scale_y_reverse(limits=c(11,0)) + xlim(0,11)# + scale_x_discrete(limits=c(0,11), labels=c(0:11)) 
                      scale_x_discrete(breaks=as.integer(colnames(Truth)), name="X [meters]")
  map.truth <- map.truth + geom_point(size=6) + geom_text(vjust=-1, hjust=-0.5, size=7)
  for(i in 2:length(Truth) ){
    a <- graph_input_truth[(i-1):i,]
    map.truth <- map.truth + geom_path(data=a, aes(x=X,y=Y),arrow=arrow(length=unit(0.4,"cm")), color="red", size=1)
  }
  
  print(map.truth)

  return (map.truth)
}

# Nx <- 10; Ny <- 2; dx <- 2; dy <- 3
# Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)    
# rownames(Truth) <- c(1:nrow(Truth)-1)*dy; colnames(Truth) <- c(1:ncol(Truth)-1)*dx
# TruthAsSequence <- c(0:9,19:10)
# 
# p <- plotFoldingPath (Truth, TruthAsSequence)
#ggsave("./plots/10x2-setting.pdf",p, height=3.5)

#############################SUCCESS BARS############################## 
expType <- c("2DvsMDS -45dbm"=1, "2DvsMDS -30dbm"=2)
expType <- c("Medium Noise"=1, "High Noise"=2)

expSize <- const2d5x4 <- const2d10x2 <- mds.5x4 <- mds.10x2 <- c()

type <- 1   # Simulation Pn= -45dbm h=15
expSize[type]     <- 1000

const2d5x4[type]  <- 816 
mds.5x4[type]     <- 909 

const2d10x2[type] <- 873 
mds.10x2[type]    <- 243 # -45 dbm

type <- 2   # Simulation Pn= -30dbm h=15
expSize[type]     <- 1000

const2d5x4[type]  <-  779
mds.5x4[type]     <-  991 

const2d10x2[type] <-  885
mds.10x2[type]    <- 0 


plotSuccess <- function (eType, saveToFile=FALSE)
{
  library(ggplot2)
  library(RColorBrewer)
  library(reshape)
  
  plotName <- names(expType[eType])
  dataNames <- c("Constrained 2-D","MDS-MAP"); dimensionNames <- c("5x4", "10x2")
  results <- matrix(data = c(
    const2d5x4[eType],const2d10x2[eType],    # PNSD - Probabilistic Node Sequence Discovery
    mds.5x4[eType],mds.10x2[eType]
  ),
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  dimensionNames)                  # Colnames
  ) 
  
  # Convert to percents:
  results <- round((results/expSize[eType]) * 100,2)
  
  results <- melt(results)
  
  colnames(results)=c("algorithm","dimension", "success")
  
  #reorder bars
  results <- within(results, algorithm <- factor(algorithm, levels = dataNames))
  results <- within(results, dimension <- factor(dimension, levels = dimensionNames))
  
  theme_update(axis.title.x = element_text(size = 14, vjust = -0.25),
               axis.title.y = element_text(size = 14, angle = 90, vjust = 0.25),
               axis.text.x = element_text(size = 12, color="black"),
               axis.text.y = element_text(size = 12, color="black"),
               title = element_text(size=12),
               panel.background = element_blank(),
               panel.grid.major = element_line(colour = "grey90"),
               legend.position = "bottom",
               legend.box = "horizontal",
               legend.key = element_blank(), 
               legend.background = element_rect(fill="white", size=0.25),
               legend.text = element_text(size=12))
  
  p <- ggplot (results, aes(x=dimension, y=success, fill=algorithm)) + geom_bar(stat="identity", position="dodge")
  p<- p + geom_bar(stat="identity", position="dodge", colour="white", show_guide=FALSE) # to hide legend slashes
  p <- p + labs(x="Grid Dimensions") + scale_y_continuous("Success [%]", limits=c(0,100))
  p <- p + scale_fill_manual(name="Algorithm", values = rev(brewer.pal(6,"YlGnBu")), guide=guide_legend(nrow=2,))
  p <- p + geom_text(data=results, aes(y=(success),label=paste(success,"%",sep="")),position=position_dodge(width=0.9), size=3, vjust=0)
  #p <- p + geom_text(data=results, aes(y=(50), label=algorithm),position=position_dodge(width=0.9), size=3.5, vjust=0, angle=90)
  p <- p + ggtitle(plotName)
  
  print(p)
  if (saveToFile)
  {
    cat(plotName, "saving...\n")
   ggsave(filename=paste("./plots/",plotName,"-2.pdf",sep=""), plot=p, width=3, height=5)
    
  }
}

plotSuccess(1, saveToFile=TRUE)
