library(ggplot2)
library(gridExtra)
require(scales)
#library("RColorBrewer")

removeSpaces <- function(...) {a<-gsub(" ","-", ... , fixed=TRUE); gsub(",","-", a , fixed=TRUE)}

source("multiplot.R")

#expID <- 10

if (expID == 1) 
{
  files <- Sys.glob("verify/verify4thFlSouthWindow2ndRowNormal/*verify*txt")
  #expName <- "4th-Fl-South-Window2ndRow-Reverse"
  expName <- "Set-C, Right-to-Left"
  nPackets <- 40
} else 
  if (expID == 2) 
  {
    files <- Sys.glob("verify/verify4thFlSouthWindow2ndRowReverse/verifyVerdicts-*.txt")
    #expName <- "4th-Fl-South-Window2ndRow-Normal"
    expName <- "Set-C, Left-to-Right"
    nPackets <- 40
  } else 
    if (expID == 3) 
    {
      files <- Sys.glob("verify/verify4thFlNorthSideWindow-interference/4thFloorNorthSide-nointerference-verify1-300.txt")
      expName <- "4th-Fl-North-Window-Normal-NoInterference"
      nPackets <- 40
    } else 
      if (expID == 4) 
      {
        files <- Sys.glob("verify/verify4thFlNorthSideWindow-interference/4thFloorNorthSide-with3interferers-verify1-300.txt")
        expName <- "4th-Fl-North-Window-Normal-With3Interferers"
        nPackets <- 40
      } else 
        if (expID == 5) 
        {
          files <- Sys.glob("verify/verify4thFlNorthSideWindow-interference/4thFloorNorthSide-with1interferers-verify301-600.txt")
          expName <- "4th-Fl-North-Window-Normal-With1Interferer"
          nPackets <- 40
        } else 
          if (expID == 6) 
          {
            files <- Sys.glob("verify/4thFlSouth1stRowFailedOnes-verify.txt")
            expName <- "4th-Fl-South-Window1stRowFailedOnly-Reverse"
            nPackets <- 40
          } else 
            if (expID == 7) 
            {
              files <- Sys.glob("verify/4thFloorNorthSide-nointerference-Reverse-verify1-300.txt")
              expName <- "4th-Fl-North-Window1stRow-Reverse-noInterferer"
              nPackets <- 40
            } else 
              if (expID == 8) 
              {
                files <- Sys.glob("verify/4thFloorNorthSide-1interferer-Reverse-verify301-600.txt")
                expName <- "4th-Fl-North-Window1stRow-Reverse-with1interferer"
                nPackets <- 40
              } else 
                if (expID == 9) 
                {
                  files <- Sys.glob("verify/4thFloorNorthSide-3interferers-Reverse-verify1-300.txt")
                  expName <- "4th-Fl-North-Window1stRow-Reverse-with3interferers"
                  nPackets <- 40
                } else 
                  if (expID == 10) 
                  {
                    files <- Sys.glob("verify/verify4thFlSouth1stRowAisleNormal/*verify*.txt")
                    expName <- "Set-A, Left-to-Right"
                    nPackets <- 40
                  } else 
                    if (expID == 11) 
                    {
                      files <- Sys.glob("verify/verify4thFlSouth1stRowAisleReverse/*verify*.txt")
                      #expName <- "4th-Fl-South-Window1stRow-Reverse"
                      expName <- "Set-A, Right-to-Left"
                      nPackets <- 40
                    } else 
                      if (expID == 12) 
                      {
                        files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/4thFloorNorth-interference2/4thFloorNorthSideWindow-3interferer-Normal/4thFloorNorthWindow-3interferers-Normal-verify1-300.txt")
                        expName <- "4th-Fl-North-Window1stRow-Normal-3interferer-2"
                        nPackets <- 40
                      } else 
                        if (expID == 13) 
                        {
                          files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/4thFloorNorth-interference2/4thFloorNorthSideWindow-1interferer-Normal/4thFloorNorthSide-1interferer-Normal-verify301-600.txt")
                          expName <- "4th-Fl-North-Window1stRow-Normal-1interferer-2"
                          nPackets <- 40
                        }else 
                          if (expID == 14) 
                          {
                            files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/4thFloorNorth-interference2/4thFloorNorthSideWindow-0interferer-Normal/4thFloorNorthSideWindow-0interferer-Normal-verify601-900.txt")
                            expName <- "4th-Fl-North-Window1stRow-Normal-0interferer-2"
                            nPackets <- 40
                          } else 
                            if (expID == 15) 
                            {
                              files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/4thFloorNorth-interference2/4thFloorNorthSideWindow-3interferer-Reverse/4thFloorNorthSideWindow-3interferer-Reverse-verify1-300.txt")
                              expName <- "4th-Fl-North-Window1stRow-Reverse-3interferer-2"
                              nPackets <- 40
                            } else 
                              if (expID == 16) 
                              {
                                files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/4thFloorNorth-interference2/4thFloorNorthSideWindow-1interferer-Reverse/4thFloorNorthSideWindow-1interferer-Reverse-verify301-600.txt")
                                expName <- "4th-Fl-North-Window1stRow-Reverse-1interferer-2"
                                nPackets <- 40
                              }else 
                                if (expID == 17) 
                                {
                                  files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/4thFloorNorth-interference2/4thFloorNorthSideWindow-0interferer-Reverse/4thFloorNorthSideWindow-0interferer-Reverse-verify601-900.txt")
                                  expName <- "4th-Fl-North-Window1stRow-Reverse-0interferer-2"
                                  nPackets <- 40
                                } else 
                                  if (expID == 18) 
                                  {
                                    files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Aisle-1Packet-Normal-verify*.txt")
                                    expName <- "4th-Fl-South-Aisle1stRow-Normal-1packet"
                                    nPackets <- 1
                                  } else 
                                    if (expID == 19) 
                                    {
                                      files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Aisle-1Packet-Reverse-verify*.txt")
                                      expName <- "4th-Fl-South-Aisle1stRow-Reverse-1packet"
                                      nPackets <- 1
                                    } else 
                                      if (expID == 20) 
                                      {
                                        files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Window-1Packet-Normal-verify*.txt")
                                        expName <- "4th-Fl-South-Window2ndRow-Normal-1packet"
                                        nPackets <- 1
                                      } else 
                                        if (expID == 21) 
                                        {
                                          files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Window-1Packet-Reverse-verify*.txt")
                                          expName <- "4th-Fl-South-Window2ndRow-Reverse-1packet"
                                          nPackets <- 1
                                        } else 
                                          if (expID == 22) 
                                          {
                                            files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Aisle-8Packet-Normal-verify*.txt")
                                            expName <- "4th-Fl-South-Aisle1stRow-Normal-8packet"
                                            nPackets <- 8
                                          } else 
                                            if (expID == 23) 
                                            {
                                              files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Aisle-8Packet-Reverse-verify*.txt")
                                              expName <- "4th-Fl-South-Aisle1stRow-Reverse-8packet"
                                              nPackets <- 8
                                            } else 
                                              if (expID == 24) 
                                              {
                                                files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Window-8Packet-Normal-verify*.txt")
                                                expName <- "4th-Fl-South-Window2ndRow-Normal-8packet"
                                                nPackets <- 8
                                              } else 
                                                if (expID == 25) 
                                                {
                                                  files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Window-8Packet-Reverse-verify*.txt")
                                                  expName <- "4th-Fl-South-Window2ndRow-Reverse-8packet"
                                                  nPackets <- 8
                                                } else 
                                                  if (expID == 26) 
                                                  {
                                                    files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Aisle-16Packet-Normal-verify*.txt")
                                                    expName <- "4th-Fl-South-Aisle1stRow-Normal-16packet"
                                                    nPackets <- 16
                                                  } else 
                                                    if (expID == 27) 
                                                    {
                                                      files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Aisle-16Packet-Reverse-verify*.txt")
                                                      expName <- "4th-Fl-South-Aisle1stRow-Reverse-16packet"
                                                      nPackets <- 16
                                                    } else 
                                                      if (expID == 28) 
                                                      {
                                                        files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Window-16Packet-Normal-verify*.txt")
                                                        expName <- "4th-Fl-South-Window2ndRow-Normal-16packet"
                                                        nPackets <- 16
                                                      } else 
                                                        if (expID == 29) 
                                                        {
                                                          files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/nPacket/4thFloorSouthSide-Window-16Packet-Reverse-verify*.txt")
                                                          expName <- "4th-Fl-South-Window2ndRow-Reverse-16packet"
                                                          nPackets <- 16
                                                        } else 
                                                          if (expID == 30) 
                                                          {
                                                            files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetA-Q50-Normal-verify1-2000.txt")
                                                            expName <- "SetA-Q50-Normal"
                                                            nPackets <- "Q50"
                                                          } else 
                                                            if (expID == 31) 
                                                            {
                                                              files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetA-Q90-verify-Normal-verify1-2000.txt")
                                                              expName <- "SetA-Q90-Normal"
                                                              nPackets <- "Q90"
                                                            } else 
                                                              if (expID == 32) 
                                                              {
                                                                files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetA-Q100-Normal-verify1-2000.txt")
                                                                expName <- "SetA-Q100-Normal"
                                                                nPackets <- "Q100"
                                                              } else 
                                                                if (expID == 33) 
                                                                {
                                                                  files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetA-Q50-Reverse-verify1-2000.txt")
                                                                  expName <- "SetA-Q50-Reverse"
                                                                  nPackets <- "Q50"
                                                                } else 
                                                                  if (expID == 34) 
                                                                  {
                                                                    files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetA-Q90-verify-Reverse-verify1-2000.txt")
                                                                    expName <- "SetA-Q90-Reverse"
                                                                    nPackets <- "Q90"
                                                                  } else 
                                                                    if (expID == 35) 
                                                                    {
                                                                      files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetA-Q100-Reverse-verify1-2000.txt")
                                                                      expName <- "SetA-Q100-Reverse"
                                                                      nPackets <- "Q100"
                                                                    } else 
                                                                      if (expID == 36) 
                                                                      {
                                                                        files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetC-Q50-Normal-verify1-2000.txt")
                                                                        expName <- "SetC-Q50-Normal"
                                                                        nPackets <- "Q50"
                                                                      } else 
                                                                        if (expID == 37) 
                                                                        {
                                                                          files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetC-Q90-verify-Normal-verify1-2000.txt")
                                                                          expName <- "SetC-Q90-Normal"
                                                                          nPackets <- "Q90"
                                                                        } else 
                                                                          if (expID == 38) 
                                                                          {
                                                                            files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetC-Q100-Normal-verify1-2000.txt")
                                                                            expName <- "SetC-Q100-Normal"
                                                                            nPackets <- "Q100"
                                                                          } else 
                                                                            if (expID == 39) 
                                                                            {
                                                                              files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetC-Q50-Reverse-verify1-2000.txt")
                                                                              expName <- "SetC-Q50-Reverse"
                                                                              nPackets <- "Q50"
                                                                            } else 
                                                                              if (expID == 40) 
                                                                              {
                                                                                files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetC-Q90-verify-Reverse-verify1-2000.txt")
                                                                                expName <- "SetC-Q90-Reverse"
                                                                                nPackets <- "Q90"
                                                                              } else 
                                                                                if (expID == 41) 
                                                                                {
                                                                                  files <- Sys.glob("/Users/ergin/phd/R/probabilisticSeq/verify/Qx/SetC-Q100-Reverse-verify1-2000.txt")
                                                                                  expName <- "SetC-Q100-Reverse"
                                                                                  nPackets <- "Q100"
                                                                                }
                                                          
                                                           

joinReports <- function(files) {
  verifyDF <- data.frame()
  for (f in files)
    verifyDF <- rbind(verifyDF, read.table(f, header=TRUE, stringsAsFactors=FALSE))
  
  expNoOrder <- with(verifyDF,order(expNo))
  verifyDF <- verifyDF[expNoOrder,]
  row.names(verifyDF) <- 1:nrow(verifyDF)
  return(verifyDF)
}

#Filename <- "verify/4thFloorNorthSide-nointerference-verify1-300.txt"
#analyse <- read.table (Filename, header=TRUE)

analyse <- joinReports(files)

#plainResults <- subset(analyse, Rank == 1 & verifyRank == 1, select=-c(5,6,7,8,9,11)) # just single result
plainResults <- subset(analyse, Rank == 1 & verifyRank == 1) # just single result
plainSuccess <- subset(plainResults, isCorrect == TRUE)
plainFail <- subset(plainResults, isCorrect == FALSE)
most.common.mistake <- names(table(plainFail$computedSeq)[1])

experiments <- unique(analyse$expNo)
success <- 0
match <- 0
truePositive <- c() # TRUE TRUE & match
falsePositive <- c() # FALSE FALSE & match

trueNegative <- c() # FALSE TRUE 
falseNegative <- c() # TRUE FALSE

for (e in experiments) {
  tExp <- subset(analyse, expNo == e)
  
  if (tExp[1,"isCorrect"])
    success <- success +1
  
  if (tExp[1,"match"])
    match <- match +1
  
  if(tExp[1,"isCorrect"] == TRUE && tExp[1,"verifyIsCorrect"] == TRUE )
    truePositive <- c(truePositive,e)
  
  if(tExp[1,"isCorrect"] == FALSE && tExp[1,"match"] == TRUE)
    falsePositive <- c(falsePositive,e)
  
  if(tExp[1,"isCorrect"] == FALSE && tExp[1,"match"] == FALSE)
    trueNegative <- c(trueNegative,e)
  
  if(tExp[1,"isCorrect"] == TRUE && tExp[1,"match"] == FALSE)
    falseNegative <- c(falseNegative,e)	
}

#matches <- subset(analyse, match == TRUE)

cat("success: ", success, "\n")
cat("match: ", match, "\n")

cat("truePositive: ", length(truePositive), "\n")
cat("falsePositive: ", length(falsePositive), "\n")
cat("trueNegative: ", length(trueNegative), "\n")
cat("falseNegative: ", length(falseNegative), "\n")
#print(expName); stop()
#plot(subset(analyse, expNo %in% truePositive & Rank == 1 & verifyRank ==1)$jointProb)

# expNos of Unreliable measurements
findUnreliables <- function(analyse) 
{
    unreliables <- c()
    #unreliables <- data.frame()
    for (e in unique(analyse$expNo))
    {
      tDF <- subset(analyse, expNo == e) # or tDF <- analyse[analyse$expNo==e,]
      seqStrings <- c(as.character(tDF$computedSeq),as.character(tDF$verifySeq))
      
      thisExpIsUnreliable <- FALSE
      for(s in seqStrings) {
        
        if (grepl("NA",s)) # contains NAs
        {
          thisExpIsUnreliable <- TRUE
          unreliables <- c(unreliables,e)
          #unreliables <- rbind(unreliables, tDF)
            break
        } 
        else
        { 
          splitted <- as.data.frame(strsplit(s,","))
          if (TRUE %in% (splitted == 0)) # contains 0s
          {
            thisExpIsUnreliable <- TRUE
            unreliables <- c(unreliables,e)
            #unreliables <- rbind(unreliables, tDF)
              break
          }
        }        
      } #for s
    } #for e
    return(unreliables)
}

unreliableExpNos <- findUnreliables(analyse)
unReliable <- subset(analyse, expNo %in% unreliableExpNos & Rank == 1 & verifyRank == 1)

data.frame() -> 
                weakResult ->     # no-match
                lowReliable ->    # n-match, rank=2
                mediumReliable -> # n-match, rank=1 OR 1-match, rank=2
                highReliable;     # 1-match, rank=1

for (e in unique(analyse$expNo)) 
{
  if(e %in% unreliableExpNos)
    next
  expDF <- subset(analyse, expNo == e)
  
  matchDegree <- sum(expDF$match)
  
  #no-match
  if(matchDegree == 0)
  {
    weakResult <- rbind(weakResult, subset(expDF, Rank == 1 & verifyRank == 1));
  }
  else if (matchDegree == 1) # 1-match
  {
    finalChoice <- subset(expDF, match == TRUE)
    
    if ( finalChoice$Rank == 1)
    {  
#       testVerdict <-subset(expDF, Rank==1)[1,"prob"]/subset(expDF, Rank==2)[1,"prob"]
#       if (testVerdict < mediumThreshold)
#         mediumReliable <- rbind(mediumReliable, finalChoice)
#       else
        highReliable <- rbind(highReliable, finalChoice)
    }
    else 
    {
      if (finalChoice$Rank == 2)
        mediumReliable <- rbind(mediumReliable, finalChoice);
    }
  } 
  else if (matchDegree > 1) # n-match, make Final Choice by max(verifyIsCorrect)
  {
    # Eski, verifyProb'u max olanlari secen. iptal.
#     matchingRows  <- expDF
#     rowWithMaxProb <- which(matchingRows==max(matchingRows$verifyProb),arr.ind=TRUE)[,"row"] 
   
    # verdict and verify arasindan probability orani buyuk olandan buyuk probabilitili olani al
    
    matchingRows <- subset(expDF,match==TRUE)
    
    testOrderProb <- with(matchingRows, order(prob,decreasing=TRUE))
    testVerdict <- matchingRows[testOrderProb,]$prob[1] / matchingRows[testOrderProb,]$prob[2]
    
    testOrderVerifyProb <- with(matchingRows, order(verifyProb,decreasing=TRUE))
    testVerify <- matchingRows[testOrderVerifyProb,]$verifyProb[1] / matchingRows[testOrderVerifyProb,]$verifyProb[2]
    
#     if (testVerdict >= testVerify) {
#       rowWithMaxProb <- which(matchingRows==max(matchingRows$prob),arr.ind=TRUE)[,"row"] 
#     } else {
#       rowWithMaxProb <- which(matchingRows==max(matchingRows$verifyProb),arr.ind=TRUE)[,"row"] 
#     }

      # MAX prob'u sec    
      if (max(matchingRows$prob) > max(matchingRows$verifyProb)) {
        rowWithMaxProb <- which(matchingRows==max(matchingRows$prob),arr.ind=TRUE)[,"row"] 
      } else 
        rowWithMaxProb <- which(matchingRows==max(matchingRows$verifyProb),arr.ind=TRUE)[,"row"] 
      
      

    finalChoice <- matchingRows[rowWithMaxProb,]
    mediumThreshold <- 1.1
    if (testVerdict < mediumThreshold || testVerify < mediumThreshold){
      lowReliable <- rbind(lowReliable,finalChoice)
    } else
      if (finalChoice$Rank == 1){
      mediumReliable <- rbind(mediumReliable,finalChoice)
    } else 
      if (finalChoice$Rank == 2)
        lowReliable <- rbind(lowReliable,finalChoice)
  
  } # if matchdegree >1
} #for e

verifiedSuccess <- sum(highReliable$isCorrect,mediumReliable$isCorrect, lowReliable$isCorrect, weakResult$isCorrect, unReliable$isCorrect)

# Combine all to summarise
# if (nrow(highReliable)>0)   highReliable$reliability    <- "high"
# if (nrow(mediumReliable)>0) mediumReliable$reliability  <- "medium"
# if (nrow(lowReliable)>0)    lowReliable$reliability     <- "low"
# if (nrow(weakResult)>0)     weakResult$reliability      <- "weak"
# if (nrow(unReliable)>0)     unReliable$reliability      <- "unreliable"

# if (nrow(highReliable)>0)   highReliable$reliability    <- "high"
# if (nrow(mediumReliable)>0) mediumReliable$reliability  <- "high"
# if (nrow(lowReliable)>0)    lowReliable$reliability     <- "medium"
# if (nrow(weakResult)>0)     weakResult$reliability      <- "medium"
# if (nrow(unReliable)>0)     unReliable$reliability      <- "low"

if (nrow(highReliable)>0)   highReliable$reliability    <- "high"
if (nrow(mediumReliable)>0) mediumReliable$reliability  <- "medium"
if (nrow(lowReliable)>0)    lowReliable$reliability     <- "medium"
if (nrow(weakResult)>0)     weakResult$reliability      <- "low"
if (nrow(unReliable)>0)     unReliable$reliability      <- "low"

# Draw histogram:
finalVerdicts <- rbind(highReliable, mediumReliable, lowReliable, weakResult, unReliable)
finalVerdicts$reliability <- factor(finalVerdicts$reliability, levels=c("high","medium", "low"))
#finalVerdicts$isCorrect <- factor(finalVerdicts$isCorrect, levels=c("TRUE","FALSE"))

finalVerdicts$numPackets <- nPackets

scale_fill_manual2 <- function(){ 
                          #return (scale_fill_manual("Verdict",values=c("#f03b20","#1f78b4")))
                          #temporarily ignore below
                          if(length(unique(finalVerdicts$isCorrect))==2) 
                            scale_fill_brewer("Verdict",palette="Paired") # Blue and light blue
                            #scale_fill_manual("Verdict",values=c("#f03b20","#1f78b4")) #Blue and Red 
                          else 
                            scale_fill_manual("Verdict",values=c("#1f78b4"))
                      }
highReliableSuccess <- sum(highReliable$isCorrect) #+ sum(mediumReliable$isCorrect)
highReliableTotal <- nrow(highReliable) #+ nrow(mediumReliable)
highReliableSuccessRatio <- round(highReliableSuccess / highReliableTotal, digits=4)*100
titleTxt <- paste(expName,
                  #"\nSuccess=",success,"/",length(experiments),"(",round(success/length(experiments),digit=4)*100,"%) ",
                  "\n Total Success =", verifiedSuccess, "/",length(experiments),"(",round(verifiedSuccess/length(experiments),digits=4)*100,"%)\n",
                  "HighReliable Success =",highReliableSuccess,"/",highReliableTotal,"(",highReliableSuccessRatio,"%)\n")


pTypes <- c("bar", "timeLapse")
pType <- pTypes[1:2]

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


if ("bar" %in% pType)
{
  p1 <- ggplot(finalVerdicts, aes(x=reliability, fill=isCorrect))
  #p1 <- p1 + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous("Verdict Ratio [%]",labels=percent) # ylab("Number Of Experiments") #+ labels(percent_format())
  p1 <- p1 + geom_bar() + scale_y_continuous("Number Of Experiments", limits=c(0,2000))
  p1 <- p1 + scale_fill_manual2() + guides(fill=guide_legend(reverse=TRUE)) 
  p1 <- p1 + ggtitle(titleTxt) + theme(panel.background=element_rect(fill="#FAFAFA"), legend.position="bottom")
  p1 <- p1 + scale_x_discrete(drop=FALSE)
  #p1 <- p1+facet_wrap(~Rank+verifyRank)
  #p1 <- p1 + stat_bin(aes(label=(..count..) ), vjust=-0.5, geom="text", position="identity") # add text above bars
  print(p1)
  #setOfPlots <- list(setOfPlots,p1)
} 
if ("timeLapse" %in% pType && FALSE) {
  if(nrow(plainFail) >0)
    plainFail$reliability <- "plainFail"
  #plainResults$reliability <- "plainResults"
  finalVerdicts <- rbind(finalVerdicts,plainFail)
  finalVerdicts$reliability <- factor(finalVerdicts$reliability, levels=c("plainFail","high","medium", "low", "weak", "unreliable","plainResults"))
  
  p2 <- ggplot(finalVerdicts, aes(x=expNo, y=reliability, fill=isCorrect))
  p2 <- p2 + geom_tile(width=1)
  # Add plainFails:
  #p2 <- p2 + geom_tile(data=plainFail, aes(x=expNo, y="plainFail", fill=isCorrect))
  #p2 <- scale_y_discrete(limits=c("a","b","c","d"))
  p2 <- p2 + scale_fill_manual2()  + guides(fill=guide_legend(reverse=TRUE))
  p2 <- p2 + ggtitle(titleTxt) + theme(panel.background=element_rect(fill="#FAFAFA"), plot.title=element_text(size=10))
  p2 <- p2 + scale_y_discrete(drop=FALSE)
#  print(p2)
  #p1 <- p1+facet_wrap(~Rank+verifyRank)
  #setOfPlots <- list(setOfPlots,p2)
}

plotName <- paste("verify/plots/3-levels-highExclusive/Thrs",mediumThreshold,"max-",if (expID<10) "0",expID,"-",expName,".pdf",sep="")
plotName <- removeSpaces(plotName)
cat("Saving", plotName,"\n")
#ggsave(plotName,p1)
print(paste(expName,
            "Success = ",success, "/",length(experiments),"(",round(success/length(experiments),digits=4)*100,"%)", " | Total verified Success =", verifiedSuccess, "/",length(experiments),"(",round(verifiedSuccess/length(experiments),digits=4)*100,"%) |",
            "HighReliable Success =",highReliableSuccess,"/",highReliableTotal,"(",highReliableSuccessRatio,"%)\n"))

 #pdf(file=plotName)
#  grid.arrange(p1,p2,ncol=1)
#   multiplot(p1,p2, cols=1)
# dev.off()
# # 
# pdf(file="verify/plots/SetA-reliability.pdf")
#   grid.arrange(p10,p11,ncol=2)
#  dev.off()

#p1 <- p1 + text(x=0, y=-50, "HighSuccess = this") + scale_fill_brewer("Verdict",palette="Paired")


if(FALSE) 
{
  comparison <- data.frame()
  comparison <- rbind(comparison, data.frame(type="noVerify", verdict="success", count=success))
  comparison <- rbind(comparison, data.frame(type="noVerify", verdict="fail", count=(length(experiments)-success)))
  comparison <- rbind(comparison,data.frame(type="withVerify", verdict="success", count=verifiedSuccess))
  comparison <- rbind(comparison,data.frame(type="withVerify", verdict="fail", count=(length(experiments)-verifiedSuccess)))
    
  p2 <- ggplot(comparison, aes(x=type, y=count, fill=verdict)) 
  p2 <- p2 + geom_bar(stat="identity") + ylab("Number Of Experiments")
  print(p)
  
  multiplot(p1,p2, cols=2)
}

# scale_fill_manual(name="Verdict", values=brewer.pal(2,"Paired"), guide=guide_legend(reverse=TRUE))
# p <- p + theme(panel.background=element_rect(fill="#F0F0F0"))
# c("#F45F5A", "blue")
# 
###################################################################################################################
# library("plyr")
# plied <- ddply (analyse, .(expNo), summarize,
#                 maxJointProb = max(jointProb),
#                 verdict = function(DF) {DF[DF$jointProb == max(DF$jointProb),]$isCorrect} )
#                 //verdict = subset(isCorrect,jointProb==maxJointProb))
# plied2 <- ddply (analyse, .(expNo), summarize,
#                 maxJointProb = max(jointProb),
#                 verdict = isCorrect)

# p <- ggplot(subset(analyse, expNo %in% truePositive & Rank == 1 & verifyRank ==1), aes(y=jointProb, x=expNo))
# p <- p + geom_point(size=1)
# print(p)
# 
# maxJPDF <- data.frame()
# for (e in unique(analyse$expNo)) {
#   xDF <- subset (analyse, expNo == e)
#   xDF <- xDF[xDF$jointProb == max(xDF$jointProb),]
#   maxJPDF <- rbind(maxJPDF,xDF)
# }
# p <- ggplot(maxJPDF, aes(y=jointProb, x=expNo, color=isCorrect))
# p <- p + geom_point(size=1) + facet_grid(~match)
# print(p)
# 
# p <- ggplot(maxJPDF, aes(x=prob, fill=match))
# p <- p + geom_histogram() + facet_wrap(~isCorrect,ncol=1, nrow=2) +ggtitle ("title")
# print(p)


# theme_update(axis.title.x = element_text(size = 12, vjust = -0.25),
#              axis.title.y = element_text(size = 12, angle = 90, vjust = 0.25),
#              axis.text.x = element_text(size = 11),
#              axis.text.y = element_text(size = 11),
#              panel.background = element_blank(),
#              panel.grid.major = element_line(colour = "grey90"),
#              legend.position = "right",
#              legend.box = "horizontal",
#              legend.key = element_blank(), 
#              legend.background = element_rect(fill="white", size=0.25),
#              legend.text = element_text(size=10))