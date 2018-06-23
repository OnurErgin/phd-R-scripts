## probabilisticSeq.R - O.Ergin - 17.06.2013
# v2 - 14.08.2013
# Description ...
# 	This one clusters sequences in subsequences of n. After n'th node in the sequence, the best one is taken and iterated from there to the end.
# 	It should be after every n'th node after the beginning or last cropping, but for our case (max node=10) this is not necessary <== resolved

# In addition, it doesn't only select the first winning node, but also other nodes with up to 2dbm difference for selecting Winning receivers.
#
# Version History: 
#
# v2.1 - 30.10.2013
#	Tree will be clustered with the paths whose P's are equal or greater than maximum P
#	Clustering is now every maxSubSeqSize mod of the path length
#
# v2.2 - 19.11.2013
#	packets is subsetted to Truth list, for the case there are more nodes in the measurement than desired.
#
# v2.3 - 05.12.2013
# Last remaining node is now assigned a heuristical probability by the last placed node, compared with the two nodes previously placed node
#
# v2.4 - 05.01.2014
# From 3rd node on, look back to update probability of last placed node
#

#library(tcltk)

## Load Functions
source("userFunctions.R")

## Load configuration file
## global/common variables are written 
## in a configuration file now.
source("configuration.R")


MatrixRowSize <- 5040 	# 7!=5040
totalSuccess <- 0

for(expNo in experimentSet)
{   
	startTime <- proc.time()
	
	TRACE_FILE <- paste(directory,inFilePrefix,expNo,inFileSuffix, sep="")
	
	if ("packets" %in% ls() && debugging) {
		; # do not reload packets
	}
	else # not debugging
	{
		cat("Loading:",TRACE_FILE)	
		packets <- read.table(TRACE_FILE, sep="\t", na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("factor",3), "numeric", "factor", "character", "numeric"), header=FALSE)
	}
	packets <- subset(packets, sender %in% Truth & receiver %in% Truth)

	## DEBUG SET
	if (debugging) {
		print("Debug set.")
		packets <- subset(packets,receiver %in% debugSet & sender %in% debugSet)
	}
	
	# Create two parallel matrices
	probabilitySeqMatrix <- matrix(data = NA, nrow = MatrixRowSize, ncol = numnodes+1, byrow = TRUE, dimnames = list(NULL,c(paste("N",1:numnodes,sep=""),"P")))
	probabilitiesMatrix <- matrix(data = 1, nrow = MatrixRowSize, ncol = numnodes, byrow = TRUE, dimnames = list(NULL,paste("P",1:numnodes,sep="")))
	
	probabilitySeqDF <- as.data.frame(probabilitySeqMatrix)
	probabilitiesDF <- as.data.frame(probabilitiesMatrix)	

	probabilitySeqDF[1,1] <- refnode
	probabilitiesDF[1,1] <- 1
	validRows <- 1
	lastRow <- 1
	
	probabilitySeqDF[1,"P"] <- 1
	
	#probabilitySeqDF$prob <- 0    #probability that's computed at the end
#	probabilitySeqDF$dynProb <- 1 #dynamic probability that grows with iterations
  
	for (currentPosition in 1:(numnodes-1)) #For each position
	{
    cat ("currentPosition: ",currentPosition,"\n")
		# Cut off the sequences here: after 5th node, take the best subseq and continue from there.
		
		#maxSubSeqSize <- 5 # now in configuration.R
		if (currentPosition %% maxSubSeqSize == 0) # Start Clustering
		{
			#cluster()
			print("clustering")

			#print (probabilitiesDF[1:validRows,])
			print (probabilitySeqDF[1:validRows,])
			#cat("Paused.. Hit Enter to continue"); scan(n=1)
			probabilitySeqDF$P[1:validRows] <- computeProbabilities(probabilitiesDF,validRows)
				
			maxProb <- max(probabilitySeqDF$P, na.rm=TRUE)
			
			#selectedRows <- probabilitySeqDF$P>=(maxProb/2)
			selectedRows <- probabilitySeqDF$P[1:validRows]>=(maxProb/2)
			newValidRows <- sum(selectedRows)
			
			cat("Cut down from", validRows, "to",newValidRows, "rows.")
			
			selectedRows <- c(selectedRows,rep(TRUE,nrow(probabilitySeqDF)-length(selectedRows)))
			
			#This code removes undesired rows, resulting in a reduced size of DFs
			probabilitiesDF <- probabilitiesDF[selectedRows,]
			probabilitySeqDF <- probabilitySeqDF[selectedRows,]
	
			validRows <- newValidRows
			
		} # Finish Clustering
    
		## Progress Bar
		#
		#cat("\n"); 
		#pb <- txtProgressBar(min = 0, max = numnodes, style = 3)
		##
    
		lastRow <- validRows
		for (Row in lastRow:1) 
		{
			s <- probabilitySeqDF[Row,currentPosition]
			#cat("\nRow: ", Row, ", s: ",s, "\n",sep=""); print(probabilitySeqDF[Row,])
				
			if (currentPosition == (numnodes-1)) 
			{
				;# Put the remaining node right away and continue
				#s <- probabilitySeqDF[Row,currentPosition]
				lastNode <- subset(packets, sender == s & !(receiver %in% probabilitySeqDF[Row,1:currentPosition]), select=c(receiver))
				
				#cat("lastNode: \n"); print(unique(lastNode));
				if(length(unique(lastNode)) > 1) 
				{
					#cat("Error: lastNode size > 1 =", length(unique(lastNode)))
					print(unique(lastNode))
					next	
				}
								
				if(length(unique(lastNode)) == 1)
				{
          if (TRUE) # Put the last remaining node to the end with Np=1
					{  
            nodeToAdd <- as.integer(as.character(lastNode[1,1])) #don't know why lastNode[1] doesn't work
            
            probabilitySeqDF[Row,currentPosition+1] <- nodeToAdd
				    probabilitiesDF[Row,currentPosition+1] <- 1
            
            currentSequence <- probabilitySeqDF[Row,1:currentPosition]
            
            if (currentPosition >= 3) 
            {
              lastNodesFreqTable <- getSendersFreqTable(packets, theSender = nodeToAdd, includeList=currentSequence)
              pReverse <- lastNodesFreqTable[as.character(s)]/sum(lastNodesFreqTable)
              if (is.na(pReverse))
              {
                cat("pReverse not found between",nodeToAdd,"->",s,"\n"); print(lastNodesFreqTable)
                pReverse <- 0
              }
              probabilitiesDF[Row,currentPosition+1] <- probabilitiesDF[Row,currentPosition+1] * pReverse
            }
          }
          else {
            lastPlacedNode <- probabilitySeqDF[Row,currentPosition]
            
            twoNodeBehind <- currentPosition-2  
            excludeList <- probabilitySeqDF[Row,c(1:currentPosition)[-twoNodeBehind]]
            
            sendersControlReceivers <- c()
            sendersControlReceivers <- getSendersWinningReceivers(packets, theSender = lastPlacedNode, excludeList=excludeList)
            sendersControlReceivers <- sendersControlReceivers[!is.na(sendersControlReceivers)]	
            freqTable <- table(sendersControlReceivers)
            
            remainingNode <- as.integer(as.character(lastNode[1,1]))  #don't know why lastNode[1] doesn't work
            
            if (as.character(remainingNode) %in% names(freqTable))
              pRemainingNode <- freqTable[names(freqTable)==as.character(remainingNode)]/sum(freqTable)
            else
              pRemainingNode <- 0
            probabilitySeqDF[Row,currentPosition+1] <- remainingNode
            probabilitiesDF[Row,currentPosition+1] <- pRemainingNode
            cat("P(",lastPlacedNode,"|",probabilitySeqDF[Row,twoNodeBehind],",",remainingNode,") =>", "P(",lastPlacedNode,"|",remainingNode,")=",pRemainingNode,"\n")
          }
			  }
			  	next

			}
		    
  		currentSequence <- probabilitySeqDF[Row,1:currentPosition]
			#find and compute probabilities here
			
      #TEST getSendersWinningReceivers(packets, theSender = s, excludeList=currentSequence),includeList=Truth)#TEST
                                 
			sendersWinningReceivers <- c()
			sendersWinningReceivers <- getSendersWinningReceivers(packets, theSender = s, excludeList=currentSequence)
			sendersWinningReceivers <- sendersWinningReceivers[!is.na(sendersWinningReceivers)]	 # remove NAs 	
		   	
	  	#cat("Row=",Row,"currentPosition=",currentPosition,"UniqueReceivers of ", s, " : ", unique(sendersWinningReceivers), "\n")
	  	
      nodeToAdd <- mostFreqReceiver(sendersWinningReceivers,1)
	  	pToAdd <- findProbability(sendersWinningReceivers,1,length(sendersWinningReceivers))
	  	
			probabilitySeqDF[Row,currentPosition+1] <- nodeToAdd 
			probabilitiesDF[Row,currentPosition+1] <- pToAdd
      
      if (currentPosition >= 3) 
      {
        lastNodesFreqTable <- getSendersFreqTable(packets, theSender = nodeToAdd, includeList=currentSequence)
        pReverse <- lastNodesFreqTable[as.character(s)]/sum(lastNodesFreqTable)
        if (is.na(pReverse))
        {
          cat("pReverse not found between",nodeToAdd,"->",s,"\n"); print(lastNodesFreqTable)
          pReverse <- 0
        }
        probabilitiesDF[Row,currentPosition+1] <- probabilitiesDF[Row,currentPosition+1] * pReverse
      }
      
	  #	cat("\nprobabilitySeqDF=\n");   print(probabilitySeqDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")
		#	                                print(probabilitiesDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")

			## FOR EACH sendersWinningReceivers ADD A ROW AND UPDATE PROBABILITY
		if (length(unique(sendersWinningReceivers)) > 1)
			for (r in 2:length(unique(sendersWinningReceivers)))
			{
				validRows <- validRows + 1
				probabilitySeqDF[validRows,] <- c(probabilitySeqDF[Row,]) #copy all row, then modify *copied* currentPosition+1
				probabilitiesDF[validRows,] <- c(probabilitiesDF[Row,])
				
				#cat("Added Row:",validRows,"\n")
				
				newRow <- validRows
				nodeToAdd <- mostFreqReceiver(sendersWinningReceivers,r)
				pToAdd <- findProbability(sendersWinningReceivers,r,length(sendersWinningReceivers))
        
				probabilitySeqDF[newRow,currentPosition+1] <- nodeToAdd
				probabilitiesDF[newRow,currentPosition+1] <- pToAdd
				
				if (currentPosition >= 3) 
				{
				  lastNodesFreqTable <- getSendersFreqTable(packets, theSender = nodeToAdd, includeList=currentSequence)
				  pReverse <- lastNodesFreqTable[as.character(s)]/sum(lastNodesFreqTable)
				  if (is.na(pReverse))
				  {
				    cat("pReverse not found between",nodeToAdd,"->",s,"\n"); print(lastNodesFreqTable)
				    pReverse <- 0
				  }
				  probabilitiesDF[Row,currentPosition+1] <- probabilitiesDF[Row,currentPosition+1] * pReverse
				}
			}
			
      ## Progress Bar
      #
      #progress <- (lastRow-Row)#*numnodes
			#setTxtProgressBar(pb, progress); cat("\n"); 
      ##cat("Progress:",progress,"\n")
      ##
      
		} #for Row in lastRow:1

	} # for currentPosition
	#close(pb)
	probabilitySeqDF$P[1:validRows] <- computeProbabilities(probabilitiesDF,validRows)
	probabilitySeqDF$prob <- 0    #probability that's computed at the end
	if (TRUE){
		for(rr in 1:validRows)
		{
			prob <- 1
			if (TRUE)
			for (cc in 1:ncol(probabilitiesDF))
			{
				#cat(" probabilitiesDF[",rr,",",cc,"]=",probabilitiesDF[rr,cc], sep="")
				prob <- prob * probabilitiesDF[rr,cc]
			}
			prob <- prod(probabilitiesDF[rr,])
			#print("\n")
			probabilitySeqDF[rr,"prob"] <- prob
			#cat("probabilitySeqDF[",rr,",prob] <- ",prob,"=>",probabilitySeqDF[rr,"prob"],"\n",sep="")
		}
		#print(probabilitySeqDF[1:validRows,]); #print(probabilitiesDF)
	}

	if (produceOutput)
	{
	  if (!file.exists(outputDirectory))
	    dir.create(outputDirectory,showWarnings=TRUE,recursive=TRUE)
    
		outFileName1 <- paste(outputDirectory,fileNamePrefix1,expNo,fileNameSuffix,sep="")
		write.table(probabilitySeqDF[1:validRows,], file=outFileName1, sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE)
		
		outFileName2 <- paste(outputDirectory,fileNamePrefix2,expNo,fileNameSuffix,sep="")
		write.table(probabilitiesDF[1:validRows,], file=outFileName2, sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE)
		
		cat(validRows,"rows written to:",outFileName1,"and",outFileName2,"\n")
	}
	
	
	## RESULT
	print(probabilitySeqDF[which.max(probabilitySeqDF$prob),])
	
	verdict <- all(probabilitySeqDF[which.max(probabilitySeqDF$prob),1:numnodes] == Truth)
	cat("Winner is:", verdict,"\n"); 
	
	## Print Elapsed Time
	endTime <- proc.time()
	print(endTime-startTime)
	
	
	if(verdict == TRUE)
	{
		totalSuccess <- totalSuccess +1
	}
	cat("TotalSUCCESS=",totalSuccess,"\n");
	
	probabilitySeqDF <- probabilitySeqDF[1:validRows,]
	probabilitiesDF <- probabilitiesDF[1:validRows,]
	
	if (FALSE){
	rm(packets)
	rm(probabilitiesDF)
	rm(probabilitySeqDF)
	rm(probabilitiesMatrix)
	rm(probabilitySeqMatrix)}
}

