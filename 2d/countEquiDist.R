cols <- Nx <- 10; rows <- Ny <- 5; dx <- 3; dy <- 3; sd_noise_dbm <- -45
titleTxt <- ""
Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
refnodes <- c(Truth[1,1],Truth[Ny,1])
#directory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
#basedirectory <- "inputs_simulation/equiDist/3x3_ideal/"; subdirectory<-""
#basedirectory <- "./equiDist/" # For Carme
basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
#basedirectory <- paste("../measurements/simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="");  subdirectory<-"" #"../measurements/simOut-5x4-3mX3m-Noise-30dbm/";
directory <- paste(basedirectory,subdirectory,sep="")


verifyfile <- paste(basedirectory,"verdict-",length(refnodes),"refs-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm.txt",sep="")
verifyDF <- read.table(verifyfile, header = TRUE)
verifyDF$reliability <- factor(verifyDF$reliability, levels=c("high","medium", "low"))

print(verifyfile)
cat("Total Success:", sum(verifyDF$verdict),"\n")
cat("High Reliable Success:", sum(subset(verifyDF, reliability=="high")$verdict), 
                  ", Fail: ",sum(subset(verifyDF, reliability=="high")$verdict == FALSE), "\n")
cat("Medium Reliable Success:", sum(subset(verifyDF, reliability=="medium")$verdict),
                  ", Fail: ",sum(subset(verifyDF, reliability=="medium")$verdict == FALSE), "\n")
cat("Low Reliable Success:", sum(subset(verifyDF, reliability=="low")$verdict), 
                  ", Fail: ",sum(subset(verifyDF, reliability=="low")$verdict == FALSE), "\n")


scale_fill_manual2 <- function(){ 
  #return (scale_fill_manual("Verdict",values=c("#f03b20","#1f78b4")))
  #temporarily ignore below
  if(length(unique(verifyDF$verdict))==2) 
    scale_fill_brewer("Verdict",palette="Paired") # Blue and light blue
  #scale_fill_manual("Verdict",values=c("#f03b20","#1f78b4")) #Blue and Red 
  else 
    scale_fill_manual("Verdict",values=c("#1f78b4"))
}

library (ggplot2)
theme_update(axis.title.x = element_text(size = 20, vjust = -0.25),
             axis.title.y = element_text(size = 20, angle = 90, vjust = 0.25),
             axis.text.x = element_text(size = 16, color = "black"),
             axis.text.y = element_text(size = 16, color = "black"),
             title = element_text(size=8),
             panel.background = element_blank(),
             panel.grid.major = element_line(colour = "grey90"),
             legend.position = "bottom",
             legend.box = "horizontal",
             legend.key = element_blank(), 
             legend.background = element_rect(fill="white", size=0.25),
             legend.text = element_text(size=16),
             legend.title = element_text(size=16))

p <- ggplot(verifyDF, aes(x=reliability, fill=verdict)) + geom_bar()
p <- p + scale_y_continuous("Number Of Experiments", limits=c(0,length(unique(verifyDF$expNo))))
p <- p + scale_fill_manual2() + guides(fill=guide_legend(reverse=TRUE)) 
p <- p + ggtitle(titleTxt) + theme(panel.background=element_rect(fill="#FAFAFA"), legend.position="bottom")
p <- p + scale_x_discrete(drop=FALSE)
print(p)

read.table("MDS_FP.DF", header = TRUE)
