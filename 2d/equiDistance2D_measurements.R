# 24/03/2015

######
#ToDo: Try three candidates for refnode selection
#      Find the limitations through RSS-curve, how right can one go on the curve, 
#        what RSS is still doable?
#

#cols <- Nx <- 5; rows <- Ny <- 4; dx <- 3; dy <- 3; sd_noise_dbm <- -45

experimentSet <- 1:10 # 3x3fails: c(8, 16, 34, 54, 59, 83, 90) # 5x5fails: c(8, 34, 41, 59, 81, 83, 90)
#Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
#refnodes <- c(Truth[1,1],Truth[Ny,1])
expTypes <- c("4x3_2mX2m","6x2_1mX3m_TableTop", "6x2_1mX3m_Floor","4x3_2mX2m_TableTop","4x3_2mX1m_TableTop")
expType <- expTypes[3]
switch(expType,
       "4x3_2mX2m" = {
              cols <- Nx <- 4; rows <- Ny <- 3; dx <- 2; dy <- 2; sd_noise_dbm <- -45
              Truth <- matrix (c(20,15,8,5,9,13,7,14,2,17,6,11), nrow=Ny, byrow=TRUE) 
              refnodes <- c(20,2)
              directory <- "./inputs_measurements/4x3_2mX2m_Floor/"
              fileprefix <- "setup2Ground5_"
              experimentSet <- 1:10
       },
       "6x2_1mX3m_TableTop" = {
         cols <- Nx <- 6; rows <- Ny <- 2; dx <- 1; dy <- 3; sd_noise_dbm <- -45
         Truth <- matrix (c(10,8,17,9,11,6,
                            7,13,2,14,15,4), nrow=Ny, byrow=TRUE) 
         refnodes <- c(10,7)
         directory <- "./inputs_measurements/6x2_1mX3m_TableTop/"
         fileprefix <- "setup1Table5_"
         experimentSet <- 1:10
       },
       "6x2_1mX3m_Floor" = {
         cols <- Nx <- 6; rows <- Ny <- 2; dx <- 1; dy <- 3; sd_noise_dbm <- -45
         Truth <- matrix (c(10,8,17,9,11,6,
                            7,13,2,14,15,4), nrow=Ny, byrow=TRUE) 
         refnodes <- c(10,7)
         directory <- "./inputs_measurements/6x2_1mX3m_Floor/"
         fileprefix <- "setup1Ground5_"
         experimentSet <- 1:10
       },
       "4x3_2mX2m_TableTop" = {
         cols <- Nx <- 4; rows <- Ny <- 3; dx <- 2; dy <- 2; sd_noise_dbm <- -45
         Truth <- matrix (c(15,20,7,2,
                            13,8,6,11,
                            14,9,5,17), nrow=Ny, byrow=TRUE) 
         refnodes <- c(2,17)
         directory <- "./inputs_measurements/data/4x3_2mX2m_TableTop/"
         fileprefix <- "onur_"
         experimentSet <- 0:9
       },
       "4x3_2mX1m_TableTop" = {
         cols <- Nx <- 4; rows <- Ny <- 3; dx <- 1; dy <- 2; sd_noise_dbm <- -45
         Truth <- matrix (c(15,20,7,2,
                            13,8,6,11,
                            14,9,5,17), nrow=Ny, byrow=TRUE) 
         refnodes <- c(15,14)
         directory <- "./inputs_measurements/data/4x3_2mX1m_TableTop/"
         fileprefix <- "onur_"
         experimentSet <- 0:9
       }
)

numnodes <- length(Truth)


getFirstColumnRefs <- function (refnodes, packets, numRows) {
  ## Discover the first column; refNodeSet
  knownNode <- refnodes[1]
  refNodeSet <- c(knownNode)
  if (numRows > 2)
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
  
  #TRACE_FILE <- paste(directory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
  TRACE_FILE <- paste(directory, fileprefix,expNo,".txt",sep="")
  cat("Loading:",TRACE_FILE, "\n")  
  packets <- read.table(TRACE_FILE,  na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("numeric",3), "numeric", "factor", "character", "numeric"), header=FALSE)
  packets$time  <- 0
  packets$power <- 0
  
  refNodeSet <- getFirstColumnRefs(refnodes, packets, Ny)  

  print(refNodeSet)
  nrowsResult <- length(refNodeSet); ncolsResult <- ceiling(numnodes/length(refNodeSet))
  result <- matrix(data=-1,nrow=nrowsResult, ncol=ncolsResult)
  
  result[,1] <- refNodeSet # place refnodes in place: along the 1st column
  # Row-by-row select the next node at each column
  for (ci in 2:ncolsResult){ # for each column
    for (ri in 1:nrowsResult){ # for each row; only odds: which(1:Ny %% 2 == 1)
     # if (ci == 3) browser()
      if (result[ri,ci] != -1) next;
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
