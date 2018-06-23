

files <- Sys.glob("probabilitySeqDF*txt")
#print(files)

setTruth <- function (TruthList,referencenode) { #print(TruthList)
	        if (TruthList[length(TruthList)] == referencenode)
	                return(rev(TruthList))
	        else
	                return(TruthList)
	        }

#Truth<-c(16,138,96,141,92,145,88,147,10,153);
Truth <- 1:10
refnode <- 1
numnodes <- 10
totalSuccess <- 0
totalChecks <- 0
#Truth <- rev(Truth)

failIndexes <- c()
failFiles   <- c()
failSequences <- matrix(data = NA, nrow = 0, ncol = numnodes, byrow = TRUE, dimnames = list(NULL,paste("N",1:numnodes,sep="")))

i <- 0
for (resultFile in files)
{
	i <- i+1
	probabilitySeqDF <- read.table(resultFile, header=TRUE)
	
	## RESULT
	winningSequence <- probabilitySeqDF[which.max(probabilitySeqDF$prob),1:numnodes] 
	verdict <- all(winningSequence == Truth)
	print(probabilitySeqDF[which.max(probabilitySeqDF$prob),]); cat("Winner is:", verdict,"\n"); 
	if(verdict == TRUE)
	{
		totalSuccess <- totalSuccess +1
	}
	else {
		failIndexes <- c(failIndexes,i)
		failFiles <- c(failFiles,resultFile)
		failSequences <- rbind(failSequences,winningSequence)
	}
	
		
	totalChecks <- totalChecks +1
	cat("TotalSUCCESS=",totalSuccess, "/",totalChecks, "in", length(files),"files\n");

}

print("Failed ones:")
print (failSequences)
print (failFiles)
#write.table(failIndexes, file="failIndexes.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE)
#write.table(failFiles, file="failFiles.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE)
#write.table(failSequences, file="failSequences.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE)
	
