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
  
	for (nextnode in 1:(numnodes-1)) #For each position
	{
		# Cut off the sequences here: after 5th node, take the best subseq and continue from there.
		
		#maxSubSeqSize <- 5 # now in configuration.R
		if (nextnode %% maxSubSeqSize == 0) # Start Clustering
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
		cat("\n"); 
		pb <- txtProgressBar(min = 0, max = numnodes, style = 3)
		##
    
		lastRow <- validRows
		for (Row in lastRow:1) 
		{
			s <- probabilitySeqDF[Row,nextnode]
			#cat("\nRow: ", Row, ", s: ",s, "\n",sep=""); print(probabilitySeqDF[Row,])
			
			if (probabilitiesDF[Row,1] == 0)
				next
				
			if (nextnode == (numnodes-2) && FALSE) {  ##  ... Ni Nj] Nk Nl ==> P(NjNk) = P(NiNk)*P(NjNk) , totally heuristical.
			  lastNodes <- subset(packets, sender == s & !(receiver %in% probabilitySeqDF[Row,1:nextnode]), select=c(receiver))
			  
        lastPlacedNode <- probabilitySeqDF[Row,nextnode]
			  excludeList <- probabilitySeqDF[Row,c(1:nextnode)]
			 
			 
        ## Last Sender
			  lastSendersReceivers <- c()
			  lastSendersReceivers <- getSendersWinningReceivers(packets, theSender = lastPlacedNode, excludeList=excludeList)
			  lastSendersReceivers <- lastSendersReceivers[!is.na(lastSendersReceivers)]  
        freqTable1 <- table(lastSendersReceivers)
        
        ## Previous Sender
			  prevSender <- probabilitySeqDF[Row,nextnode-1]
			  prevSendersReceivers <- c()
			  prevSendersReceivers <- getSendersWinningReceivers(packets, theSender = prevSender, excludeList=excludeList)
			  prevSendersReceivers <- prevSendersReceivers[!is.na(prevSendersReceivers)]  
			  
			  freqTable2 <- table(prevSendersReceivers)
        
        print ("BEFORE")
        cat ("LastNode:",lastPlacedNode,"\n")
        print(freqTable1)
        cat("PreviousNode:", prevSender,"\n")
        print(freqTable2)
			  print(probabilitySeqDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")
			  print(probabilitiesDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")
        rcvrNo <- 1
        for (r in unique(lastSendersReceivers))
        {
          pr1 <- freqTable1[as.character(r)]/sum(freqTable1)
          pr2 <- freqTable2[as.character(r)]/sum(freqTable2)
          
          if (rcvrNo == 1) 
          {
            probabilitySeqDF[Row,nextnode+1] <- as.character(r)
            probabilitiesDF[Row,nextnode+1] <- pr1*pr2
            rcvrNo <- rcvrNo + 1
          } 
          else 
          {
            validRows <- validRows + 1
            probabilitySeqDF[validRows,] <- c(probabilitySeqDF[Row,]) 
            probabilitiesDF[validRows,] <- c(probabilitiesDF[Row,])
            
            probabilityDFLastIndex <- validRows
            probabilitySeqDF[probabilityDFLastIndex,nextnode+1] <- as.character(r)
            
            #find and compute probabilities here
            nodeProbability <- pr1*pr2
            probabilitiesDF[probabilityDFLastIndex,nextnode+1] <- nodeProbability 
          }
          print ("AFTER")
          print(probabilitySeqDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")
          print(probabilitiesDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")
          
        }
        
			  
			} 
      else 
			if (nextnode == (numnodes-1)) 
			{
				;# Put the remaining node right away and continue
				#s <- probabilitySeqDF[Row,nextnode]
				lastNode <- subset(packets, sender == s & !(receiver %in% probabilitySeqDF[Row,1:nextnode]), select=c(receiver))
				
				#cat("lastNode: \n"); print(unique(lastNode));
				if(length(unique(lastNode)) > 1) 
				{
					#cat("Error: lastNode size > 1 =", length(unique(lastNode)))
					print(unique(lastNode))
					next	
				}
								
				if(length(unique(lastNode)) == 1)
				{
          if (FALSE) # Put the last remaining node to the end with Np=1
					{  
            probabilitySeqDF[Row,nextnode+1] <- as.integer(as.character(lastNode[1,1])) #don't know why lastNode[1] doesn't work
				    probabilitiesDF[Row,nextnode+1] <- 1
          }
          else {
            lastPlacedNode <- probabilitySeqDF[Row,nextnode]
            
            #remainingNode <- lastNode
            #twoNodeBehind <- probabilitySeqDF[Row,nextnode-2]
            #includeList <- c(remainingNode,twoNodeBehind)
            twoNodeBehind <- nextnode-2  
            excludeList <- probabilitySeqDF[Row,c(1:nextnode)[-twoNodeBehind]]
            
            sendersControlReceivers <- c()
            
            sendersControlReceivers <- getSendersWinningReceivers(packets, theSender = lastPlacedNode, excludeList=excludeList)
            
            sendersControlReceivers <- sendersControlReceivers[!is.na(sendersControlReceivers)]	
            
            remainingNode <- as.integer(as.character(lastNode[1,1]))  #don't know why lastNode[1] doesn't work
            
            freqTable <- table(sendersControlReceivers)
            if (as.character(remainingNode) %in% names(freqTable))
              pRemainingNode <- freqTable[names(freqTable)==as.character(remainingNode)]/sum(freqTable)
            else
              pRemainingNode <- 0
            probabilitySeqDF[Row,nextnode+1] <- remainingNode
            probabilitiesDF[Row,nextnode+1] <- pRemainingNode
            cat("P(",lastPlacedNode,"|",probabilitySeqDF[Row,twoNodeBehind],",",remainingNode,") =>", "P(",lastPlacedNode,"|",remainingNode,")=",pRemainingNode,"\n")
          }
			  }
			  	next

			}
		    
		currentSequence <- probabilitySeqDF[Row,1:nextnode]
			#find and compute probabilities here
			
			sendersWinningReceivers <- c()
	
			sendersWinningReceivers <- getSendersWinningReceivers(packets, theSender = s, excludeList=currentSequence)

			sendersWinningReceivers <- sendersWinningReceivers[!is.na(sendersWinningReceivers)]		  	
		  
      maxReceiver <- mostFreqReceiver(sendersWinningReceivers)
		  maxReceiverProb <- findProbability(sendersWinningReceivers,maxReceiver,length(sendersWinningReceivers[!is.na(sendersWinningReceivers)]))
	  	
	  	
	  	# FOR EACH sendersWinningReceivers ADD A ROW AND UPDATE PROBABILITY
	  	
	  	#cat("Row=",Row,"nextnode=",nextnode,"UniqueReceivers of ", s, " : ", unique(sendersWinningReceivers), "\n")
	  	
	  	uniqueSenders<-unique(sendersWinningReceivers)
	  	#cat("\nAdd node:", uniqueSenders[1])
	  	#probabilitySeqDF[i,nextnode+1] <- uniqueSenders[1]
	  	probabilitySeqDF[Row,nextnode+1] <- mostFreqReceiver(sendersWinningReceivers,1)
	  	probabilitiesDF[Row,nextnode+1] <- findProbability(sendersWinningReceivers,1,length(sendersWinningReceivers[!is.na(sendersWinningReceivers)]))
	  	
	  	#probabilitySeqDF[Row,"P"] <- probabilitySeqDF[Row,"P"] * probabilitiesDF[Row,nextnode+1]
	  	
	  	cat("\nprobabilitySeqDF=\n");   print(probabilitySeqDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")
			                                #print(probabilitiesDF[1:validRows,]); cat("\b (",validRows," rows)\n",sep="")


		if (length(unique(sendersWinningReceivers)) > 1)
			for (r in 2:length(unique(sendersWinningReceivers)))
			{
				#probabilitySeqDF <- rbind(probabilitySeqDF, c(probabilitySeqDF[Row,])) #,receiversOfInterest[r])	)
				#probabilitiesDF <- rbind(probabilitiesDF, c(probabilitiesDF[Row,]))
				validRows <- validRows + 1
				probabilitySeqDF[validRows,] <- c(probabilitySeqDF[Row,]) #,receiversOfInterest[r])	)
				probabilitiesDF[validRows,] <- c(probabilitiesDF[Row,])
				
				#cat("Added Row:",validRows,"\n")
				
				#Depreciated: probabilityDFLastIndex <- nrow(probabilitySeqDF)
				
				probabilityDFLastIndex <- validRows
				probabilitySeqDF[probabilityDFLastIndex,nextnode+1] <- mostFreqReceiver(sendersWinningReceivers,r)
				
				#find and compute probabilities here
				nodeProbability <- findProbability(sendersWinningReceivers,r,length(sendersWinningReceivers[!is.na(sendersWinningReceivers)]))
				probabilitiesDF[probabilityDFLastIndex,nextnode+1] <- nodeProbability
				
				#Update the probability
				#probabilitySeqDF[probabilityDFLastIndex,"prob"] <- nodeProbability * probabilitySeqDF[probabilityDFLastIndex,"prob"]
			}
			
      ## Progress Bar
      #
      progress <- (lastRow-Row)#*numnodes
			setTxtProgressBar(pb, progress); cat("\n"); 
      ##cat("Progress:",progress,"\n")
      ##
		}
    
	}
	close(pb)
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

