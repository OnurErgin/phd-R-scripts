# 21/05/2015
# NICE TRY, BUT USELESS

######
#ToDo: Try three candidates for refnode selection
#      Find the limitations through RSS-curve, how right can one go on the curve, 
#        what RSS is still doable?
#

cols <- Nx <- 5; rows <- Ny <- 4; dx <- 3; dy <- 3; sd_noise_dbm <- -30

experimentSet <- 1:100 # 3x3fails: c(8, 16, 34, 54, 59, 83, 90) # 5x5fails: c(8, 34, 41, 59, 81, 83, 90)
Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
refnodes <- c(Truth[1,1],Truth[Ny,1])

numnodes <- length(Truth)
#directory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
#basedirectory <- "inputs_simulation/equiDist/"
basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
directory <- paste(basedirectory,subdirectory,sep="")

getFirstColumnRefs <- function (refnodes, packets, numRows) {
  ## Discover the first column; refNodeSet
  knownNode <- refnodes[1]
  refNodeSet <- c(knownNode)
  for (ri in 1:(numRows-2)){
    # Choose two candidates at 90 degrees to each other
    winners <- getSendersWinningReceivers(allPackets=packets,theSender=knownNode,excludeList=refNodeSet,includeList=NULL)
    firstCandidate <- mostFreqReceiver(valueSet=winners,tIndex=1)
    secondCandidate <- mostFreqReceiver(valueSet=winners,tIndex=2)
    
    if(refnodes[2] %in% c(firstCandidate,secondCandidate)){ # Arrived at the second reference node; with for loop this must never be TRUE
      #refNodeSet <- c(refNodeSet, refnodes[2])
      #print(refNodeSet)
      break
    }
    
    # Choose the closest one of the two cantidates to the other refnode
    winners <- getSendersWinningReceivers(allPackets=packets,theSender=refnodes[2],excludeList=NULL,includeList=c(firstCandidate,secondCandidate))
    knownNode <- mostFreqReceiver(winners, tIndex= 1)
    #cat (knownNode, "with P=", findProbability(winners[1,], 1, length(winners[1,])),"\n")
    refNodeSet <- c(refNodeSet, knownNode)
    #print(refNodeSet)
  }
  refNodeSet <- c(refNodeSet, refnodes[2]) # With for loop, only nodes between refnodes are discovered
  return(refNodeSet)
}

pruneThreshold <- 2
source("probSeqFunctions-fixedPrune.R")

topQuantile <- 1
rssMaxDifference <- 0 

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
  
  #refNodeSet <- getFirstColumnRefs(refnodes, packets, Ny)  
  refNodeSet <- as.character(c(0,5,10,15))

  print(refNodeSet)
  nrowsResult <- length(refNodeSet); ncolsResult <- ceiling(numnodes/length(refNodeSet))
  result <- matrix(data=-1,nrow=nrowsResult, ncol=ncolsResult)
  
  result[,1] <- refNodeSet # place refnodes in place: along the 1st column
  # Row-by-row select the next node at each column
  for (ci in 2:ncolsResult){ # for each column
    for (ri in 1:nrowsResult){ # for each row; only odds: which(1:Ny %% 2 == 1)
      #if (ri == 2 && ci == 4) browser()
      if (result[ri,ci] != -1) next;
      winners <- getSendersWinningReceivers(allPackets=packets, theSender=result[ri,(ci-1)], excludeSet <- as.integer(result))
      closestNode1 <- mostFreqReceiver(winners,tIndex=1)
      closestNode2 <- mostFreqReceiver(winners,tIndex=2)
      
      # NICE TRY, BUT USELESS
      
      if (is.null(closestNode1)) 
        closestNode1 <- -1
      else if (ri + 1 <= Ny)
      {
        if (ri + 2 <= Ny) 
        {
          checkSender <- result[ri+2, ci-1]
          getWhich <- 2
        } 
        else if (ri - 2 >= 1) 
        {
          checkSender <- result[ri-2, ci-1]
          getWhich <- 1
        } 
        else if (ri + 1 <= Ny)
        {
          checkSender <- result[ri+1, ci-1]
          getWhich <- 2
        } 
        else if (ri - 1 >= 1) 
        {
          checkSender <- result[ri-1, ci-1]
          getWhich <- 1
        }
        winners <- getSendersWinningReceivers(allPackets=packets,theSender=checkSender,excludeList=NULL,includeList=c(closestNode1,closestNode2))
        bestNode <- mostFreqReceiver(winners, tIndex=getWhich)
        result[ri,ci] <- bestNode 
      } else {
        result[ri,ci] <- closestNode1
      } 
      
    }
  }
  class(result) <- "integer"
  print(result)
  
  if (all.equal(Truth,result) == TRUE) {
    totalSuccess <- totalSuccess + 1
    isSuccess <- TRUE
  } else {
    failedExpNos <- c(failedExpNos,expNo)
    isSuccess <- FALSE
  }
  cat("Result is ", isSuccess, "\t total: ",totalSuccess, "/",length(failedExpNos)+totalSuccess, "\n")
  
  ## Print Elapsed Time
  endTime <- proc.time()
  print(endTime-startTime)

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
  
  
} # for expNo

cat("totalSuccess=",totalSuccess,"/",length(experimentSet) ,"\n")
cat("High Reliable = ", length(highReliable), "\n")
cat("Medium Reliable = ", length(mediumReliable), "\n")
cat("Low Reliable = ", length(lowReliable), "\n")

if(FALSE)
{
  outfile <- paste(basedirectory,"verdict-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm.txt",sep="")
  write.table(verdicts, file=outfile, sep=" ", col.names=TRUE, row.names=FALSE)
  cat(outfile, " written.\n")
}
