library(ggplot2)

removeSpaces <- function(...) gsub(" ","", ... , fixed=TRUE)

joinReports <- function(files) {
  verifyDF <- data.frame()
  for (f in files)
    verifyDF <- rbind(verifyDF, read.table(f, header=TRUE, stringsAsFactors=FALSE))
  
  expNoOrder <- with(verifyDF,order(expNo))
  verifyDF <- verifyDF[expNoOrder,]
  row.names(verifyDF) <- 1:nrow(verifyDF)
  return(verifyDF)
}

readExpFileRange <- function (sourceFolder, fileSet) 
{
  packets <- data.frame()
  for (i in fileSet)
  {
    TRACE_FILE<-paste(sourceDir,"seq16ch_",i,".txt",sep="")
    expPackets <- read.table(TRACE_FILE, sep="\t", na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("numeric",3), "numeric", "factor", "character", "numeric"), header=FALSE, stringsAsFactors=FALSE)
    expPackets$run <- i
    packets<-rbind(packets,expPackets)
  }
  return(packets)
}

expID <- 3
if (expID == 1) 
{
  files <- Sys.glob("../probabilisticSeq/verify/verify4thFlSouthWindow2ndRowNormal/*verify*txt")
  expName <- "4th-Fl-South-Window2ndRow-Normal"
  sourceDir <- "/Users/ergin/phd/R/measurements/4thFloor/chOuterLoop4thFl-2ndRow10Nodes/"
} else 
  if (expID == 2) 
  {
    files <- Sys.glob("../probabilisticSeq/verify/verify4thFlSouthWindow2ndRowReverse/verifyVerdicts-*.txt")
    expName <- "4th-Fl-South-Window2ndRow-Reverse"
    sourceDir <- "/Users/ergin/phd/R/measurements/4thFloor/chOuterLoop4thFl-2ndRow10Nodes/"
  } else 
    if (expID == 3) 
    {
      files <- Sys.glob("../probabilisticSeq/verify/verify4thFlSouth1stRowAisleReverse/4thFloor1stRow-verify*.txt")
      expName <- "4th-Fl-South-Window1stRow-Reverse"
      sourceDir <- "/Users/ergin/phd/R/measurements/4thFloor/FourthFloor_chLoop2000withPktID/"
    } else if (expID == 4) 
    {
      files <- Sys.glob("../probabilisticSeq/verify/4thFloorNorthSide-3interferers-Reverse-verify1-300.txt")
      expName <- "4th-Fl-North-Window1stRow-Reverse"
      sourceDir <- "/Users/ergin/phd/R/measurements/interferenceMeasurements/2D-NorthSide/withinterference/3interferers/"
    }

analyse <- joinReports(files)

plainResults <- subset(analyse, Rank == 1 & verifyRank == 1, select=-c(5,6,7,8,9,11)) # just single result
plainSuccess <- subset(plainResults, isCorrect == TRUE)
plainFail <- subset(plainResults, isCorrect == FALSE)
most.common.mistake.str <- names(table(plainFail$computedSeq)[order(table(plainFail$computedSeq),decreasing=TRUE)][1])
most.common.mistake <- as.numeric(strsplit(most.common.mistake.str,",")[[1]])
Truth <- as.numeric(strsplit(plainSuccess[1,"computedSeq"],",")[[1]])



rng <- 40
success.range <- plainSuccess[1:rng,"expNo"]
most.common.fails <- subset(plainFail, computedSeq == most.common.mistake.str)
fail.range <- most.common.fails[1:rng,"expNo"] # consecutive olanlardan bul.

expRange <- fail.range
#expRange <- 620:660 ; expType <- "Successful"
expRange <- 579:619 ; 
expType <- "Failed"

expRange <- 15; expType <- "Single Exp"
senderNode <- 16


readExperimentsFromFile <- TRUE
if (readExperimentsFromFile)
  packets <- readExpFileRange(sourceDir,expRange)
  
sendersPackets <- subset(packets,sender==senderNode, select=c("receiver","rssi", "run","channel"))
#sendersPackets <- rbind(sendersPackets,c(senderNode,0,expRange[1]))
#packets <- rbind(packets,c(senderNode,max(packets$rssi),expRange[1]))


# Plot measurement values
if (TRUE) {
  mainTitle <- paste("Values from", expType, "Measurements \n Sender node: ", senderNode)
  pdfFileName <- paste(removeSpaces(mainTitle),".pdf",sep="")
  pngFileName <- paste(removeSpaces(mainTitle),".png",sep="")
  #mainTitle <- paste(mainTitle,"\nNorth Side")
  
  #Here comes plotting measurements
  
  #levels(packets$receiver) <- as.character(Truth)
  sendersPackets$receiver <- factor(sendersPackets$receiver, levels=rev(as.character(Truth)))
  if (FALSE)
  {
    sortedDF <- as.data.frame(matrix(data = NA, nrow = 0, ncol = ncol(sendersPackets), byrow = TRUE, dimnames = list(NULL,colnames(sendersPackets))))
    for (i in Truth)
    {
      subpackets = subset(sendersPackets, receiver == i)
      sortedDF <- rbind(sortedDF,subpackets)
    }
    sendersPackets <- sortedDF
  }
  
  #plot(packets$receiver,packets$rssi,pch=20, ylab="RSSI", xlab="Node Id (not distance sorted)"); title(mainTitle)
  p <- ggplot(sendersPackets, aes(x=receiver, y=rssi, fill=receiver))
  p <- p + stat_boxplot(geom='errorbar') # for horizontal lines on boxplot borders
  p <- p + geom_boxplot() + scale_y_continuous(breaks=-105:-42, limits=c(-105,-42), expand=c(0,0))# ylim(-105,-42)  
  #p <- p + geom_point() + stat_summary(fun.y=mean, geom="line", aes(group=1))  + stat_summary(fun.y=mean, geom="point")
  p <- p + scale_x_discrete(drop=FALSE)
  p <- p + ggtitle(paste(mainTitle,"\nRange",expRange[1],"-",expRange[length(expRange)]))
  p <- p + geom_vline(xintercept=1, color="darkblue", linetype = "longdash")
  print(p)
  #ggsave (paste(expType,"40exps-",senderNode,".pdf",sep=""));
 
  if (TRUE) #This is for the paper
  {
    mainTitle <- "Values from measurements"
    pdfFileName <- paste(removeSpaces(mainTitle),".pdf",sep="")
    pdf(pdfFileName)
     par(mar=c(5,5,4,2))
    plot(sendersPackets$receiver,sendersPackets$rssi,pch=20, ylab="RSS(dbm)", xlab="Node Id (distance sorted)", ylim=c(-105,-42), cex.lab=2, cex.axis=1.3); title(mainTitle, cex.main=2)
    dev.off()
  }
  theme_update (
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = theme_blank(),
    panel.background = element_blank(),
    axis.title = element_text(size=30),
    axis.text = element_text(size=16),
    plot.title = element_text(size=30, face="bold", vjust=3)
    #plot.margin = unit(c(1,1,1,1), "cm")
  )
  
  p <- ggplot(sendersPackets, aes(x=receiver, y=rssi, group=receiver))
  p <- p + stat_boxplot(geom='errorbar') + geom_boxplot()
  p <- p + scale_x_discrete("Node Id (distance sorted)", drop=FALSE) #+
   p <- p+scale_y_continuous("RSS[dBm]",expand = c(0,0.5), limits = c(-105,-40), breaks=floor(seq(-110,-40,by=10)))
  #p <- p + geom_hline(yintercept=radioSensitivity, color="orange") + annotate("text",label="Sensitivity Threshold",y=radioSensitivity+2, x=3, color="orange")
  p <- p + ggtitle("Values from measurements")
  print(p)
  ggsave(pdfFileName, width=7, height=7)
  # Convert pdf to png
  #system(paste("sips -s format png",pdfFileName,"--out",pngFileName))
}