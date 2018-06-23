#reilability.R
# 08.07.2015

source("EdgeDiscovery.R")

if ("package:ggplot2" %in% search())
  detach("package:ggplot2", unload=TRUE)

library("ggplot2")

cols <- Nx <- 5; rows <- Ny <- 4; dx <- 3; dy <- 3; sd_noise_dbm <- -30

topQuantile <- 0.5
rssMaxDifference <- 3 
source("probSeqFunctions-fixedPrune.R")

run.verbose <- FALSE
produceOutput <- FALSE # for probabilitySeqDF and probabilitiesDF. This file will produce output anyways

experimentSet <- 1:100 # 3x3fails: c(8, 16, 34, 54, 59, 83, 90) # 5x5fails: c(8, 34, 41, 59, 81, 83, 90)

  expSize  <- length(experimentSet)
  
  Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
  refnodes <- c(Truth[1,1],Truth[Ny,1])
  #refnodes <- c(Truth[1,1],Truth[2,1])
  numnodes <- length(Truth)
  
  this.hostname <- Sys.info()['nodename']
  
  #directory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
  #basedirectory <- "inputs_simulation/equiDist/3x3_ideal/"; subdirectory<-""
  
  if (grepl("carme", this.hostname)){
    basedirectory <- "./equiDist/" # For Carme
  } else if (grepl("Onurs-MacBook-Pro",this.hostname))
    basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
  
  subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
  #basedirectory <- paste("../measurements/simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="");  subdirectory<-"" #"../measurements/simOut-5x4-3mX3m-Noise-30dbm/";
  directory <- paste(basedirectory,subdirectory,sep="")
  
verifyWithEdges <- function(produceOutput = TRUE, numrefnodes = 2) {
  
  resdirectory <- paste(directory, ifelse (numrefnodes == 2, "results-2CornerRef/","results-3CornerRef/") , sep="")
 
  totalSuccess <- 0
  failedExpNos <- c()
  verdicts <- data.frame(expNo=numeric(expSize), verdict=logical(expSize), reliability=character(expSize), stringsAsFactors=FALSE)
  highReliable <- mediumReliable <- lowReliable <- c()
  
  overallTime <- proc.time()
  for(expNo in experimentSet)
  {   
    
  startTime <- proc.time()
      TRACE_FILE <- paste(directory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
      cat("Loading:",TRACE_FILE, "\n")  
      packets <- read.table(TRACE_FILE,  na.strings="", header=TRUE)
      packets$time  <- 0
      packets$power <- 0
      
      resultFile <- paste(resdirectory,"result-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
      cat("Loading:",resultFile, "\n")  
      result <- read.table(resultFile,  na.strings="", header=FALSE)
      
    if (all.equal(as.data.frame(Truth),result) == TRUE) {
      totalSuccess <- totalSuccess + 1
      isSuccess <- TRUE
    } else {
      failedExpNos <- c(failedExpNos,expNo)
      isSuccess <- FALSE
     # print(result)
    }
    cat("Result is ", isSuccess, "\t total: ",totalSuccess, "/",length(failedExpNos)+totalSuccess, "\n")
    
    ## Reliability
    score <- 0
    refNodeSet <- result[,1]
    # First column reverse:
    revRefNodeSet <- as.integer(getFirstColumnRefs(rev(refnodes),packets, Ny))
    if (TRUE == all.equal(rev(refNodeSet),revRefNodeSet)) {
      print("First column matches")
      score <- score + 1
    }
    
    if (-1 %in% result) {
      score <- 0
    } else { #cols <- ncolsResult ; rows <- nrowsResult
    
    lastColumn <- result[,cols]
    
    lastColumnRefnodes <- c(result[1,cols], result[rows,cols])
    lastColumnRefnodeset <- as.integer(getFirstColumnRefs(lastColumnRefnodes, packets, Ny))
    reverseLastColumnRefnodeset <- as.integer(getFirstColumnRefs(rev(lastColumnRefnodes), packets, Ny))
    
    if (TRUE == all.equal(as.integer(lastColumnRefnodeset),lastColumn) || 
        TRUE == all.equal(as.integer(reverseLastColumnRefnodeset),rev(lastColumn)) ){
      print("Last column matches")
      score <- score + 1
      
      if (TRUE == all.equal(lastColumnRefnodeset,rev(reverseLastColumnRefnodeset)))
        score <- score + 1
    }
    }
    
    if (score == 3)
    {
      highReliable <- c( highReliable , expNo)
      cat("ExpNo:",expNo," reliability is: High!\n")
      reliability <- "high"
    } else if (score == 2){
      mediumReliable <- c(mediumReliable, expNo)
      cat("ExpNo:",expNo," reliability is: Medium!\n")
      reliability <- "medium"
    } else {
      lowReliable <- c(lowReliable, expNo)
      cat("ExpNo:",expNo," reliability is: Low!\n")
      reliability <- "low"
    }
    
    verdicts[expNo,"expNo"] <- expNo
    verdicts[expNo,"verdict"] <- isSuccess
    verdicts[expNo,"reliability"] <- reliability
    print(tail(verdicts[verdicts$expNo!=0,],n=1))
   
    ## Print Elapsed Time
    endTime <- proc.time()
    print(endTime-startTime) 
  }
  ## Print Elapsed Time
  overallEndTime <- proc.time()
  print(overallEndTime - overallTime)
  
  if(produceOutput)
    write.table(verdicts,paste(resdirectory,"verdicts-edgeVerify.txt",sep=""),col.names = TRUE, row.names = FALSE)
  
  return(verdicts)
} # verify2refnodes()

findCorner <- function (packets, cornerNodes, excludes=c(-1)) 
{
  #
  # cornerNodes[]: 
  # [1][3]
  # [2]
  #
  cornerRef <- cornerNodes[1]; sideRefs <- cornerNodes[2:3]
  winners1 <- getSendersWinningReceivers(allPackets=packets, theSender=cornerRef, excludeList=as.integer(excludes))
  #closestNodes <- mostFreqReceiver(winners,tIndex=1)
  
  #winners2 <- getSendersWinningReceivers(allPackets=packets, theSender=sideRefs[1], excludeList=as.integer(excludes))
  #winners3 <- getSendersWinningReceivers(allPackets=packets, theSender=sideRefs[2], excludeList=as.integer(excludes))
  winners2 <- getSendersWinningReceivers(allPackets=packets, theSender=sideRefs[1], includeList=winners1[1,])
  winners3 <- getSendersWinningReceivers(allPackets=packets, theSender=sideRefs[2], includeList=winners1[1,])
  
  #mostCommonReceiver <- as.integer(names(table(winners2[1,winners2[1,]%in%winners3[1,]])[1]))
  
  allWinners <- c(winners2[1,],winners3[1,])
  mostFrequentReceiver <- as.integer(names(rev(sort(table(allWinners)))[1]))
  
  return(mostFrequentReceiver)
}

verify3refnodes <- function(produceOutput = TRUE, onlyError = FALSE, numrefnodes = 3) {
  
  resdirectory <- paste(directory, ifelse (numrefnodes == 2, "results-2CornerRef/","results-3CornerRef/") , sep="")
  
  totalSuccess <- 0
  failedExpNos <- c()
  verdicts <- data.frame(expNo=numeric(expSize), verdict=logical(expSize), reliability=character(expSize), stringsAsFactors=FALSE)
  highReliable <- mediumReliable <- lowReliable <- c()
  
  manhattenError <- 0
  biggestError <- c(expNo=0, Error=0)
  
  overallTime <- proc.time()
  for(expNo in experimentSet)
  {   
    startTime <- proc.time()
    
    if (!onlyError) {
      TRACE_FILE <- paste(directory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
      cat("Loading:",TRACE_FILE, "\n")  
      packets <- read.table(TRACE_FILE,  na.strings="", header=TRUE)
      packets$time  <- 0
      packets$power <- 0
    }
    
    resultFile <- paste(resdirectory,"result-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
    cat("Loading:",resultFile, "\n")  
    result <- read.table(resultFile,  na.strings="", header=FALSE)
    
    if (all.equal(as.data.frame(Truth),result) == TRUE) {
      totalSuccess <- totalSuccess + 1
      isSuccess <- TRUE
    } else {
      failedExpNos <- c(failedExpNos,expNo)
      isSuccess <- FALSE
      # print(result)
    }
    
    err <- getError(result,Truth)
    manhattenError <- manhattenError + err
    if (err > biggestError["Error"])
      biggestError <- c(expNo = expNo, Error = err)
    
    if (onlyError)
      next;
    
    #inner corners: [Ny-1,2] , [Ny-1,Nx-1], [2,Nx-1]
    cToFind <- c(result[Ny-1,2] , result[Ny-1,Nx-1], result[2,Nx-1])
    corner1 <- findCorner(packets, c(result[Ny,1],result[Ny-1,1],result[Ny,2])) 
    corner2 <- findCorner(packets, c(result[Ny,Nx],result[Ny,Nx-1],result[Ny-1,Nx]))
    corner3 <- findCorner(packets, c(result[1,Nx],result[1,Nx-1],result[2,Nx]))

    
    #outer corners: [Ny,1], [Ny,Nx], [1,Nx]
    #cToFind <- c(result[Ny,1], result[Ny,Nx], result[1,Nx])
#     corner1 <- findCorner(packets, c(result[Ny-1,2],result[Ny-1,1],result[Ny,2])) 
#     corner2 <- findCorner(packets, c(result[Ny-1,Nx-1],result[Ny,Nx-1],result[Ny-1,Nx]))
#     corner3 <- findCorner(packets, c(result[2,Nx-1],result[1,Nx-1],result[2,Nx]))
# delete     score <- 0
# delete     if (corner1 == result[Ny,1]) score <- score + 1
# delete     if (corner2 == result[Ny,Nx]) score <- score + 1
# delete     if (corner3 == result[1,Nx]) score <- score + 1
    
    score <- 0
    if (corner1 == cToFind[1]) score <- score + 1
    if (corner2 == cToFind[2]) score <- score + 1
    if (corner3 == cToFind[3]) score <- score + 1
    
    cat("1: bottom left corner",corner1); cat(" : ",corner1 == cToFind[1] ,"\n")
    cat("2: bottom right corner",corner2); cat(" : ",corner2 == cToFind[2] ,"\n")
    cat("3: top right corner",corner3); cat(" : ", corner3 == cToFind[3] ,"\n")
    
      
    if (score == 3)
    {
      highReliable <- c( highReliable , expNo)
      cat("ExpNo:",expNo," reliability is: High!\n")
      reliability <- "high"
    } else if (score == 2){
      mediumReliable <- c(mediumReliable, expNo)
      cat("ExpNo:",expNo," reliability is: Medium!\n")
      reliability <- "medium"
    } else {
      lowReliable <- c(lowReliable, expNo)
      cat("ExpNo:",expNo," reliability is: Low!\n")
      reliability <- "low"
    }
    
    verdicts[expNo,"expNo"] <- expNo
    verdicts[expNo,"verdict"] <- isSuccess
    verdicts[expNo,"reliability"] <- reliability
    #print(tail(verdicts[verdicts$expNo!=0,],n=1))
    
    
    cat("Result is ", isSuccess, reliability, "\t total: ",totalSuccess, "/",length(failedExpNos)+totalSuccess, "\n")
    
    #Reliability
    
    ## Print Elapsed Time
    endTime <- proc.time()
    print(endTime-startTime) 
  }
  cat("Total Success", totalSuccess, "\n")
  cat("Mean Manhattan Error:", manhattenError, "\n")
  cat("Biggest Error:"); print(biggestError)
  
  cat("Mean Error: ", manhattenError / (length(experimentSet) - totalSuccess ))
  
  ## Print Elapsed Time
  overallEndTime <- proc.time()
  print(overallEndTime - overallTime)
  
  if (produceOutput)
    write.table(verdicts,paste(resdirectory,"verdicts-cornerVerify.txt",sep=""),col.names = TRUE, row.names = FALSE)
  
  return(verdicts)
}

plotVerdict <- function (finalVerdicts, titleTxt){
  scale_fill_manual2 <- function(){ 
    #return (scale_fill_manual("Verdict",values=c("#f03b20","#1f78b4")))
    #temporarily ignore below
    if(length(unique(finalVerdicts$verdict))==2) 
      scale_fill_brewer("Verdict",palette="Paired") # Blue and light blue
    #scale_fill_manual("Verdict",values=c("#f03b20","#1f78b4")) #Blue and Red 
    else 
      scale_fill_manual("Verdict",values=c("#1f78b4"))
  }
  theme_update(axis.title.x = element_text(size = 20, vjust = -0.25),
               axis.title.y = element_text(size = 20, angle = 90, vjust = 0.25),
               axis.text.x = element_text(size = 16, color = "black"),
               axis.text.y = element_text(size = 16, color = "black"),
               title = element_text(size=8),
               panel.background = element_blank(),
               panel.grid.major = element_line(colour = "grey90"),
               panel.background = element_rect(fill=NA, color="black"),
               legend.position = "bottom",
               legend.box = "horizontal",
               legend.key = element_blank(), 
               legend.background = element_rect(fill="white", size=0.25),
               legend.text = element_text(size=16),
               legend.title = element_text(size=16),
               strip.background = element_blank(),
               strip.text = element_text(size=14)
               )
#browser()
#a <- finalVerdicts; 
#finalVerdicts <- a
#finalVerdicts <- subset(finalVerdicts,noise==-30)
#finalVerdicts <- subset(finalVerdicts, select = -expNo) #aes(x=noise, y=success, fill=name)
  p1 <- ggplot(finalVerdicts, aes(x=reliability, fill=verdict))
  #p1 <- p1 + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous("Verdict Ratio [%]",labels=percent) # ylab("Number Of Experiments") #+ labels(percent_format())
  p1 <- p1 + geom_bar() + facet_grid(. ~ noise)
  p1 <- p1 + scale_y_continuous("Number Of Experiments", limits=c(0,length(unique(finalVerdicts$expNo))))
  p1 <- p1 + scale_x_discrete("Reliability",drop=FALSE) 
  p1 <- p1 + scale_fill_manual2() + guides(fill=guide_legend(reverse=TRUE)) 
  p1 <- p1 + ggtitle(titleTxt)# + theme(panel.background=element_rect(fill="#FAFAFA"), legend.position="bottom")
  #p1 <- p1+facet_wrap(~Rank+verifyRank)
  #p1 <- p1 + stat_bin(aes(label=(..count..) ), vjust=-0.5, geom="text", position="identity") # add text above bars
  print(p1)
  return (p1)
}

readResults <- function (refnodes = 3, verificationtype = "edge" ) { # "edge" or "corner"
  #dx <- 3; dy <- 3; 
  #basedirectory <- "./equiDist/" # For Carme
  #basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
  #basedirectory <- paste("../measurements/simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="");  subdirectory<-"" #"../measurements/simOut-5x4-3mX3m-Noise-30dbm/";
  if (verificationtype == "edge")
    verdict_filename <- "verdicts-edgeVerify.txt"
  else if (verificationtype == "corner")
    verdict_filename <- "verdicts-cornerVerify.txt"
  
  sd_noise_dbm <- -45
  
  subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
  directory <- paste(basedirectory,subdirectory,sep="")
  if (refnodes == 2)
    resdirectory <- paste(directory, "results-2CornerRef/", sep="")
  else if (refnodes == 3)
    resdirectory <- paste(directory, "results-3CornerRef/", sep="")
  
  verdicts_45 <- read.table(paste(resdirectory,verdict_filename,sep=""), header = TRUE)
  verdicts_45$noise <- "Moderate Noise"
  
  #basedirectory <- "./equiDist/" # For Carme
  #basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
  #basedirectory <- paste("../measurements/simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="");  subdirectory<-"" #"../measurements/simOut-5x4-3mX3m-Noise-30dbm/";
  sd_noise_dbm <- -30
  subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
  directory <- paste(basedirectory,subdirectory,sep="")
  if (refnodes == 2)
    resdirectory <- paste(directory, "results-2CornerRef/", sep="")
  else if (refnodes == 3)
    resdirectory <- paste(directory, "results-3CornerRef/", sep="")
  
  verdicts_30 <- read.table(paste(resdirectory,verdict_filename,sep=""), header = TRUE)
  verdicts_30$noise <- "High Noise"
  
  verdicts <- rbind (verdicts_30,verdicts_45)
  verdicts$reliability <- factor(verdicts$reliability, levels=c("high","medium","low"))
  
  cat("Nx=",Nx, " Ny=",Ny, " dx=",dx," dy=",dy," sd_noise_dbm=",sd_noise_dbm, "\n")
  cat("Refnodes: ", refnodes, "\n")
  cat("Success 45:", sum(verdicts_45$verdict), ", Success 30:", sum(verdicts_30$verdict),"\n")
  
  return (verdicts)
}

getError <- function (result, Truth) {
  Error<- matrix(rep(0,nrow(Truth)*ncol(Truth)),nrow=nrow(Truth), ncol=ncol(Truth)) # Comparison matrix
  
  for (r in 1:nrow(result))
    for(c in 1:ncol(result))
    {
      resultLoc <- which(Truth==result[r,c], arr.ind=TRUE)
      diffRow <- abs(resultLoc[1,"row"]-r)
      diffCol <- abs(resultLoc[1,"col"]-c)
      Error[r,c] <- sum(diffRow, diffCol)
    }
  return (mean(Error))
}

savePlotToFile <- function (plot1, plotName) {
  plotfilename <- paste("./inputs_simulation/equiDist/plots/",plotName,".pdf",sep="")
  cat(plotName, "saving...",plotfilename,"\n")
  ggsave(filename=plotfilename, plot=p, width=6.86, height=5)
}

#verify2refnodes()
#verdicts_2ref <- readResults(5,4)
#plotVerdict(verdicts_2ref, "")

#verify3refnodes(FALSE, onlyError = TRUE)
#verdicts_3ref <- verify2refnodes(produceOutput = FALSE)
#verifyWithEdges(produceOutput = TRUE, numrefnodes = 3)
#verdicts_3ref <- readResults(3, verificationtype = "edge")
NumRefNodes <- 3
verdicts <- readResults( NumRefNodes, verificationtype = "edge")
plotName <- paste(Nx,"x",Ny,"-",dx,"mx",dy,"m-",NumRefNodes,"refs-reliability", sep="") 
p <- plotVerdict(verdicts, "")
savePlotToFile(p,plotName)
