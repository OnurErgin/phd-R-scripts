## Edge Discovery Functions:
#  Take two nodes and construct a Sequence 

discoverEdge <- function (refnodes, packets, numRows, excluding = c(), withP = FALSE) {
  ## Discover the first column; refNodeSet
  #browser()
  selection_stats <- c(correct=0, total=0)
  
  knownNode <- refnodes[1]
  refNodeSet <- c(knownNode)
  P <- 1 # Empirical probability of the sequence
  for (ri in 1:(numRows-2)){
    # Choose two candidates at 90 degrees to each other
    winners1 <- getSendersWinningReceivers(allPackets=packets,theSender=knownNode,excludeList=c(refNodeSet,excluding),includeList=NULL)
    firstCandidate <- mostFreqReceiver(valueSet=winners1,tIndex=1)
    secondCandidate <- mostFreqReceiver(valueSet=winners1,tIndex=2)
    
    if(refnodes[2] %in% c(firstCandidate,secondCandidate)){ # Arrived at the second reference node; with for loop this must never be TRUE
      #refNodeSet <- c(refNodeSet, refnodes[2])
      #print(refNodeSet)
     ;# break
    }
    
    # Choose the closest one of the two cantidates to the other refnode
    winners2 <- getSendersWinningReceivers(allPackets=packets,theSender=refnodes[2],excludeList=NULL,includeList=c(firstCandidate,secondCandidate))
    knownNode <- mostFreqReceiver(winners2, tIndex= 1)
    #cat (knownNode, "with P=", findProbability(winners[1,], 1, length(winners[1,])),"\n")
    if (withP) 
      P <- P * findProbability(winners2[1,], 1, length(winners2[1,])) * findProbabilityByName(winners1[1,],knownNode)
    refNodeSet <- c(refNodeSet, knownNode)
    
#     if (TrueRow[1] != -1) {
#       selection_stats["total"] <- selection_stats["total"] + 1
#       if (TrueRow[length(refNodeSet)] == knownNode){
#         cat("Selection is Correct:",TrueRow, "X", refNodeSet,"\n")
#         selection_stats["correct"] <- selection_stats["correct"] + 1
#       }
#     }
    #print(refNodeSet)
  }
#print(selection_stats)
  refNodeSet <- c(refNodeSet, refnodes[2]) # With for loop, only nodes between refnodes are discovered
  class(refNodeSet) <- "integer"
  if (withP)
    refNodeSet <- c(refNodeSet,P)
  return(refNodeSet)
}
#discoverEdge(refnodes[c(1,3)], packets, Nx)

discoverEdge.adjacentRefNodes <- function (refnodes, packets, numRows) {
  ## Discover the first column; refNodeSet
  #browser()
  knownNodes <- refnodes
  edgeNodeSet <- c(knownNodes)
  P <- 1
  for (ri in 1:(numRows-2)){
    # Choose two candidates at 90 degrees to each other
    winners1 <- getSendersWinningReceivers(allPackets=packets,theSender=knownNodes[2],excludeList=edgeNodeSet,includeList=NULL)
    candidates <- c(mostFreqReceiver(valueSet=winners1,tIndex=1), mostFreqReceiver(valueSet=winners1,tIndex=2))
    
    # Choose the closest one of the two cantidates to the other refnode
    winners2 <- getSendersWinningReceivers(allPackets=packets,theSender=knownNodes[1],excludeList=NULL,includeList=candidates)
    NOTnextNode <- mostFreqReceiver(winners2, tIndex= 1)
    nextNode <- candidates[candidates != NOTnextNode][1]
    P <- P * findProbability(winners2[1,], 1, length(winners2[1,])) * findProbabilityByName(winners1[1,],nextNode)
    #cat (knownNode, "with P=", findProbability(winners[1,], 1, length(winners[1,])),"\n")
    edgeNodeSet <- c(edgeNodeSet, nextNode)
#     if (TrueRow[1] != -1) {
#       selection_stats["total"] <- selection_stats["total"] + 1
#       if (TrueRow[length(edgeNodeSet)] == nextNode){
#         #cat("Selection is Correct!\n")
#         selection_stats["correct"] <- selection_stats["correct"] + 1
#       }
#     }
    #print(refNodeSet)
    knownNodes <- c(edgeNodeSet[length(edgeNodeSet)-1],edgeNodeSet[length(edgeNodeSet)])
  }
  #refNodeSet <- c(refNodeSet, refnodes[2]) # With for loop, only nodes between refnodes are discovered
  class(edgeNodeSet) <- "integer"
  edgeNodeSet <- c(edgeNodeSet,P)
  return(edgeNodeSet)
}

##### Below function is used.

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

getFirstColumnRefs.adjacentRefNodes <- function (refnodes, packets, numRows) {
  ## Discover the first column; refNodeSet
  #browser()
  knownNodes <- refnodes
  edgeNodeSet <- c(knownNodes)
  for (ri in 1:(numRows-2)){
    # Choose two candidates at 90 degrees to each other
    winners <- getSendersWinningReceivers(allPackets=packets,theSender=knownNodes[2],excludeList=edgeNodeSet,includeList=NULL)
    candidates <- c(mostFreqReceiver(valueSet=winners,tIndex=1), mostFreqReceiver(valueSet=winners,tIndex=2))
    
    # Choose the closest one of the two cantidates to the other refnode
    winners <- getSendersWinningReceivers(allPackets=packets,theSender=knownNodes[1],excludeList=NULL,includeList=candidates)
    NOTnextNode <- mostFreqReceiver(winners, tIndex= 1)
    nextNode <- candidates[candidates != NOTnextNode][1]
    #cat (knownNode, "with P=", findProbability(winners[1,], 1, length(winners[1,])),"\n")
    edgeNodeSet <- c(edgeNodeSet, nextNode)
    #print(refNodeSet)
    knownNodes <- c(edgeNodeSet[length(edgeNodeSet)-1],edgeNodeSet[length(edgeNodeSet)])
  }
  #refNodeSet <- c(refNodeSet, refnodes[2]) # With for loop, only nodes between refnodes are discovered
  return(edgeNodeSet)
}
