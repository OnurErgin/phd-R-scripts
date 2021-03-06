# 24/03/2015

######
#ToDo: Try three candidates for refnode selection
#      Find the limitations through RSS-curve, how right can one go on the curve, 
#        what RSS is still doable?
#

cols <- Nx <- 10; rows <- Ny <- 5; dx <- 3; dy <- 3; sd_noise_dbm <- -30

experimentSet <- 1:100 # 3x3fails: c(8, 16, 34, 54, 59, 83, 90) # 5x5fails: c(8, 34, 41, 59, 81, 83, 90)
Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
refnodes <- c(Truth[1,1],Truth[Ny,1])
#refnodes <- c(Truth[1,1],Truth[2,1])

numnodes <- length(Truth)
#directory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
#basedirectory <- "inputs_simulation/equiDist/3x3_ideal/"; subdirectory<-""
basedirectory <- "./equiDist/" # For Carme
#basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
#basedirectory <- paste("../measurements/simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="");  subdirectory<-"" #"../measurements/simOut-5x4-3mX3m-Noise-30dbm/";
directory <- paste(basedirectory,subdirectory,sep="")

source("EdgeDiscovery.R")

pruneThreshold <- 2
source("probSeqFunctions-fixedPrune.R")

topQuantile <- 0.5
rssMaxDifference <- 3 

run.verbose <- FALSE
produceOutput <- FALSE # for probabilitySeqDF and probabilitiesDF. This file will produce output anyways

expSize <- length(experimentSet)
verdicts <- data.frame(expNo=numeric(expSize), verdict=logical(expSize), reliability=character(expSize), stringsAsFactors=FALSE)
highReliable <- mediumReliable <- lowReliable <- c()
totalSuccess <- 0
failedExpNos <- c()
lostCandidateToCompetitor <- c()
for(expNo in experimentSet)
{   
  startTime <- proc.time()
  
  TRACE_FILE <- paste(directory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
  cat("Loading:",TRACE_FILE, "\n")  
  packets <- read.table(TRACE_FILE,  na.strings="", header=TRUE)
  packets$time  <- 0
  packets$power <- 0
  
  #refNodeSet <- getFirstColumnRefs.adjacentRefNodes(refnodes, packets, Ny)  
  refNodeSet <- getFirstColumnRefs(refnodes, packets, Ny)  
  #refNodeSet <- as.character(c(0,5,10,15))

  print(refNodeSet)
  nrowsResult <- length(refNodeSet); ncolsResult <- ceiling(numnodes/length(refNodeSet))
  result <- matrix(data=-1,nrow=nrowsResult, ncol=ncolsResult)
  
  result[,1] <- refNodeSet # place refnodes in place: along the 1st column
  # Row-by-row select the next node at each column
  for (ci in 2:ncolsResult){ # for each column
    for (ri in 1:nrowsResult){ # for each row; only odds: which(1:Ny %% 2 == 1)
     # if (ci == 3) browser()
      if (result[ri,ci] != -1) next;
      if (FALSE){
      winners <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri,(ci-1)], excludeSet <- as.integer(result))
      closestNode <- mostFreqReceiver(winners,tIndex=1)
      if (is.null(closestNode)) 
        closestNode <- -1
      #cat (closestNode, "with P=", findProbability(winners[1,], 1, length(winners[1,])),"\n")
      if (ri + 1 <= nrowsResult) { # if next row exists
        #####
        closestNodeP <- findProbabilityByName(winners[1,],name=closestNode)
        secondClosestNode <- mostFreqReceiver(winners,tIndex=2)
      
        nextNodeWinners <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri+1,(ci-1)], excludeSet <- as.integer(result))
        closestNodeForNextNode <- mostFreqReceiver(nextNodeWinners,tIndex=1)
        closestNodeForNextNodeP <- findProbabilityByName(nextNodeWinners[1,],name=closestNodeForNextNode)
        
        if (closestNodeForNextNode != closestNode) {
          result[ri,ci] <- closestNode 
        }else 
        if (closestNodeForNextNode == closestNode) {
          if (closestNodeP >= closestNodeForNextNodeP) {
            result[ri,ci] <- closestNode 
          } else {
            result[ri+1,ci] <- closestNodeForNextNode # ?= closestNode
            # Select a new one
            winners <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri,(ci-1)], excludeSet <- as.integer(result))
            closestNode <- mostFreqReceiver(winners,tIndex=1)
            
            if (is.null(closestNode)) 
              closestNode <- -1
              
              result[ri,ci] <- closestNode 
            
            print("Lost candidate to Competitor"); lostCandidateToCompetitor <- c(lostCandidateToCompetitor, expNo)
          }
        }
        
        #####
      } else {
        result[ri,ci] <- closestNode 
      } #if ...
      } #if FALSE
      else { # Version 2
      winners <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri,ci-1], excludeList=as.integer(result))
      candidates1 <- c(mostFreqReceiver(winners,tIndex=1),mostFreqReceiver(winners,tIndex=2))
      candidates1P <- c(findProbabilityByName(winners[1,],candidates1[1]),findProbabilityByName(winners[1,],candidates1[2]))
      
      if (ri +1 <= nrowsResult){ # if there is a row down below
        winners2 <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri+1,ci-1], excludeList=as.integer(result))
        candidates2 <- c(mostFreqReceiver(winners2,tIndex=1),mostFreqReceiver(winners2,tIndex=2))
        candidates2P <- c(findProbabilityByName(winners2[1,],candidates2[1]),findProbabilityByName(winners2[1,],candidates2[2]))
        
        
        if(sum(is.na(candidates1P)) == 0) # if 
          if(candidates1P[1]==candidates1P[2] ){
            result[ri+1,ci] <- candidates2[1]
            
            winners <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri,ci-1], excludeList=as.integer(result))
            candidates1 <- mostFreqReceiver(winners,tIndex=1)
            #candidates1P <- findProbabilityByName(winners[1,],candidates1)
            rm("candidates2")
          }
      }
      result[ri,ci] <- candidates1[1]
      rm("candidates1","winners")
      }
    }
  }
  class(result) <- "integer"
  
  if (all.equal(Truth,result) == TRUE) {
    totalSuccess <- totalSuccess + 1
    isSuccess <- TRUE
  } else {
    failedExpNos <- c(failedExpNos,expNo)
    isSuccess <- FALSE
    print(result)
  }
  cat("Result is ", isSuccess, "\t total: ",totalSuccess, "/",length(failedExpNos)+totalSuccess, "\n")
  
  ## Print Elapsed Time
  endTime <- proc.time()
  print(endTime-startTime)

  outputToFile <- TRUE
  outDir <- "results-2CornerRef/"
  outputDirectory <- paste(directory,outDir,sep="")
  
  if (!file.exists(outputDirectory) && outputToFile)
    dir.create(outputDirectory,showWarnings=TRUE,recursive=TRUE)
  
  if(outputToFile){
    outFile <- paste(outputDirectory,"result-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
    write.table(result, outFile, row.names=FALSE, col.names = FALSE)
  }
  
  if(outputToFile){
    outFile <- paste(outputDirectory,"success-",Nx,"x",Ny,"-",dx,"mX",dy,"m",".txt",sep="")
    write.table(totalSuccess, outFile, row.names=FALSE, col.names = FALSE)
  }
  
  if (FALSE){
        ## Reliability
        score <- 0
        # First column reverse:
        revRefNodeSet <- getFirstColumnRefs(rev(refnodes),packets, Ny)
        if (TRUE == all.equal(rev(refNodeSet),revRefNodeSet)) {
          print("First column matches")
          score <- score + 1
        }
        
        if (-1 %in% result) {
          score <- 0
          } else { cols <- ncolsResult ; rows <- nrowsResult
            
            lastColumn <- result[,cols]
            
            lastColumnRefnodes <- c(result[1,cols], result[rows,cols])
            lastColumnRefnodeset <- getFirstColumnRefs(lastColumnRefnodes, packets, Ny)
            reverseLastColumnRefnodeset <- getFirstColumnRefs(rev(lastColumnRefnodes), packets, Ny)
              
            if (TRUE == all.equal(as.integer(lastColumnRefnodeset),lastColumn) || 
                  TRUE == all.equal(as.integer(reverseLastColumnRefnodeset),lastColumn) ){
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
        ## Reliability:
        #First column (fc) ref nodes match reverse (rfc)
        #Last column(lc) in refnode check = lr
        #Last column reverse refnode check = lrr
        #Lastcolumn in lr or lrr
        
        # if (fc == rfc && lc %in% c(lr,lrr))
        #   if (lr==lrr) then high
        #   else medium
        # 3 comparisons. 3 matches = high, 2 matches = medium, less matches = low reliablity
        #   1: fc == rfc
        #   2: lc %in% c(lr,lrr)
        #   3: lr==lrr
        
  } # if (FALSE) Reliability
  
} # for expNo

cat("totalSuccess=",totalSuccess,"/",length(experimentSet) ,"\n")
cat("High Reliable = ", length(highReliable), "\n")
cat("Medium Reliable = ", length(mediumReliable), "\n")
cat("Low Reliable = ", length(lowReliable), "\n")

if(FALSE)
{
  outfile <- paste(directory,"verdict-",length(refnodes),"refs-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm.txt",sep="")
  write.table(verdicts, file=outfile, sep=" ", col.names=TRUE, row.names=FALSE)
  cat(outfile, " written.\n")
}
