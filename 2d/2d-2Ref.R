#2D.R

library(plyr)
#library(reshape)
#library(MASS)

topDebugLevel <- 0

rows <- 8
cols <- 2
refnode <- 100
placedNodes <- c()
#arr <- array (1:8, dim=c(2,10,2))
#myprint(arr,level=0,debugLevel=topDebugLevel)

readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}

myprint <- function (..., level=0, debugLevel = 1, separator=" ")
{
	wholestring <- list(...)
	main_print <- paste(wholestring,sep=separator) # convert to 'cat()' printable string
	foreprint <- paste("<",level,">",sep="")
	if (level >= debugLevel)
		cat(foreprint, main_print,"\n", sep=separator)
}

d_path_loss <- function(pr, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl0 <- -20*log10(lamda/(4*pi*d0));
  d0*10^((pt-pr-pl0)/(10*n))
}

# Returns the best 'numWinners' nodes according to "constraint"
getWinners <- function(node, stats, excludeSeq, argConstraint, numWinners=2) {
	senderPackets <- subset(stats,!(receiver %in% excludeSeq) & (sender == node))
	nodeWinners <- head(senderPackets[order(-senderPackets[,argConstraint]),],numWinners)[,c("receiver")]
	nodeWinners <- as.numeric(as.character(nodeWinners))
	return(nodeWinners)
}

getClosestIn <- function (node, stats, includeSet, argConstraint, numClosest=1) {
	senderPackets <- subset(stats,(receiver %in% includeSet) & (sender == node))
	closestNode <- senderPackets[order(-senderPackets[,argConstraint]),"receiver"][numClosest]
	closestNode <- as.numeric(as.character(closestNode))
	return(closestNode)
}

twoBy7 <- c(152,151,13,12,150,149,90,89,143,144,94,93,140,139)
#twoBy7 <- c(151, 153,12,10,149,147,89,88,144,145,93,92,139,141)
Truth <- matrix(twoBy7,nrow=2,ncol=length(twoBy7)/2)
refnode1 <- twoBy7[1]
refnode2 <- twoBy7[2]

directory <- "./chOuterLoop4thFl-4x7/"
filePrefix <- "seq16ch_"
fileSuffix <- ".txt"
file <- "seq16ch_10.txt"
csvfile <- "distance_vector_stats_all_channels_4x7.csv.txt"
if (!("Big_all_stats" %in% ls()) && TRUE) {
	myprint("Loading Big_all_stats",level=1,debugLevel=topDebugLevel)
	Big_all_stats <- read.table (csvfile, sep=',', colClasses=c(rep("factor",2),rep("numeric",11)), header=TRUE)
	#Big_all_stats <- read.csv (csvfile, header=TRUE) #Works but with all factors
	#Big_all_stats <- read.csv (csvfile, header=TRUE, colClasses=c(rep("factor",2),rep("numeric",11)))
	
	Big_all_stats <- subset(Big_all_stats, sender %in% twoBy7 & receiver %in% twoBy7)		
	Big_all_stats <- droplevels(Big_all_stats)
}

constraint <- "avg_max_5"
#constraint <- "max_rssi"

runBegin <- 1
runEnd   <- 693

#fileBegin <- fileEnd <- 10

RUNS <- runBegin:runEnd
#RUNS <- 693
#RUNS <- 100:300
cMaxError <- c()
cMeanError <- c()

for (RUN in RUNS)
{	
	myprint("|===============",level=1,debugLevel=topDebugLevel)
	myprint("RUN:",RUN,level=2,debugLevel=topDebugLevel)
	all_stats <- subset(Big_all_stats, run == RUN)
	
	if (!("packets" %in% ls()) && FALSE)
	{
		#file <- paste(directory,filePrefix,RUN,fileSuffix,sep="")
		Sfile <- paste(directory,file,sep="")
		packets <- read.table(file, sep="\t", na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("factor",3), "numeric", "factor", "character", "numeric"), header=FALSE)
		
		packets <- subset(packets, sender %in% twoBy7 & receiver %in% twoBy7)
		
		packets <- droplevels(packets)
	
		all_stats<-ddply(packets, .(sender,receiver), summarise,
		                packets=length(rssi),
		                max_rssi=max(rssi), count_max_rssi=sum(rssi==max(rssi)),
		                min_rssi=min(rssi),
		                median_rssi=median(rssi),
		                avg_rssi=mean(rssi),
		                sd_rssi=sd(rssi),
		                avg_max_2=mean(subset(rssi, rssi>=quantile(rssi, 0.98))),
		                avg_max_5=mean(subset(rssi, rssi>=quantile(rssi, 0.95))),
		                avg_max_10=mean(subset(rssi, rssi>=quantile(rssi, 0.90)))
		                )
		
		#all_stats_m <- melt(all_stats, variable_name="dv")
		if (FALSE){
			all_stats_rssi <- subset(all_stats, select=c(sender, receiver, max_rssi)) 
			
			all_stats_rssi_distance <- mutate(all_stats_rssi, distance=d_path_loss(pr=max_rssi, n=3))
			
			run_rssi_distance <- all_stats_rssi_distance
			
			run_rssi_distance_c <- cast(run_rssi_distance, receiver ~ sender , value="distance", fill=50.0, add.missing=TRUE)
			
			run_rssi_distance_d <-as.dist(run_rssi_distance_c)
			mds_node_coordinates<-`cmdscale`(run_rssi_distance_d,k=2)
			
			#plot(mds_node_coordinates[,1]~mds_node_coordinates[,2],xlab="X Coordinates(m)",ylab="Y Coordinates(m)")
			#text(mds_node_coordinates[,1]~mds_node_coordinates[,2], labels=row.names(mds_node_coordinates),pos=3)
		}
		#sequence<-names(sort(mds_node_coordinates[,1]))
	}
	if(FALSE)
	for (s in unique(all_stats$sender)) 
	{
		substat <- subset(all_stats,sender == s)
		r <- head(substat[order(-substat[,constraint]),],4)[,c("receiver",constraint)]
		r[,1] <- as.character(r[,1])
		cat("sender",s,":","\n",sep=" ")
		print(r)
	}
	nodeseq <- c()

	nodeseq <-c(nodeseq, c(refnode1, refnode2))
	
#	senderPackets <- subset(all_stats,sender == refnode)
#	senderPackets <- senderPackets[order(-senderPackets[,constraint]),c("receiver",constraint)] # ordered
	
	# First two nodes are chosen by the refnode
	node1 <- getWinners(refnode1,all_stats,nodeseq,constraint,numWinners=1)
	nodeseq <-c(nodeseq, node1);
	node2 <- getWinners(refnode1,all_stats,nodeseq,constraint,numWinners=2)
	nodeseq <-c(nodeseq, node2) #add nodes to nodeseq
		myprint(nodeseq,level=0,debugLevel=topDebugLevel)
		
	prevnode1 <- node1
	prevnode2 <- node2
	
	for(i in 1:10)
	{
		if(length(nodeseq)<length(twoBy7)){
			node1<-getWinners(prevnode1,all_stats,nodeseq,constraint,numWinners=1)
			myprint(prevnode1, "adds node1:",node1)
			nodeseq<-c(nodeseq,node1)
			prevnode1<-node1
		}
		else
			break
		
		if(FALSE) #paralel gitme
		if(length(nodeseq)<length(twoBy7)) {
			node2<-getWinners(prevnode2,all_stats,nodeseq,constraint,numWinners=1)
			myprint(prevnode2, "adds node2:",node2)
			nodeseq<-c(nodeseq,node2)
			prevnode2<-node2
		}

		if (TRUE) #çaprazlama
		if(length(nodeseq)<length(twoBy7)) {
			prevnode1 <- nodeseq[length(nodeseq)]
			prevnode2 <- nodeseq[length(nodeseq)-1]
			
			
			pn1Winners <- getWinners(prevnode1,all_stats,nodeseq,constraint,numWinners=2)
			pn2Winners <- getWinners(prevnode2,all_stats,nodeseq,constraint,numWinners=2)
					
			pn3Winners<-c()	
			if (TRUE) # TRUE for 3lü çaprazlama
			{			
				prevnode3 <- nodeseq[length(nodeseq)-2]
				pn3Winners <- getWinners(prevnode3,all_stats,nodeseq,constraint,numWinners=2)
			}			
			winners <- c(pn1Winners,pn2Winners,pn3Winners)
			winnerFreq <- table(winners)
			 if (max(winnerFreq) > 1)
			{
				node2 <- as.numeric(names(which.max(winnerFreq)))
			} else{
				node2<-getWinners(prevnode2,all_stats,nodeseq,constraint,numWinners=1)	
			}
			#node2<-getWinners(prevnode2,all_stats,nodeseq,constraint,numWinners=1)
			myprint(prevnode2, "adds node2:",node2)
			nodeseq<-c(nodeseq,node2)
			#prevnode2<-node2
		}
		else 
			break
	}
	
	# Iterate loop from here 
	if (FALSE)
	for  (i in 1:10)
	{
		myprint(i,level=0,debugLevel=topDebugLevel)
		winners <-c()
		#senderPackets <- subset(all_stats,!(receiver %in% nodeseq) & (sender == node1))
		#node1Winners <- head(senderPackets[order(-senderPackets[,constraint]),],2)[,c("receiver")]
		#node1Winners <- as.numeric(as.character(node1Winners))
		node1Winners <- getWinners(node1, all_stats, nodeseq, constraint, numWinners=2)
		
		#senderPackets <- subset(all_stats,!(receiver %in% nodeseq) & (sender == node2))
		##senderPackets <- senderPackets[order(-senderPackets$max_rssi),c("receiver","max_rssi")] # ordered
		#node2Winners <- head(senderPackets[order(-senderPackets[,constraint]),],2)[,c("receiver")]
		#node2Winners <- as.numeric(as.character(node2Winners))
		node2Winners <- getWinners(node2, all_stats, nodeseq, constraint, numWinners=2)
		
		
		winners <- c(node1Winners,node2Winners)
		if(length(unique(winners)) <= 1) #last node
		{
			if (length(unique(winners)) != 0)
			{
				nodeseq <- c(nodeseq,winners[1]) #add last node
				myprint(nodeseq,level=1,debugLevel=topDebugLevel)
			}
			break;
		}else
		{
			winnerFreq <- table(winners)
			 if (max(winnerFreq) > 1)
			{
				closestCornerNode <- as.numeric(names(which.max(winnerFreq)))
			}else #ask the previous node
			{
				previousNode <- nodeseq[length(nodeseq)-2]
				#senderPackets <- subset(all_stats,(receiver %in% winners) & (sender == previousNode))
				#closestCornerNode <- senderPackets[order(-senderPackets[,constraint]),"receiver"][1]
				#closestCornerNode <- as.numeric(as.character(closestCornerNode))
				closestCornerNode <- getClosestIn(previousNode, all_stats, winners, constraint, 1)
				myprint(node1," and ",node2," asked the previous node:", previousNode, level=1, debugLevel=topDebugLevel)
				#stop(node1," and ",node2," asked the previous node:", previousNode, " and it picked: ", closestCornerNode)
			}
			nodeseq <- c(nodeseq,closestCornerNode) #add
			
			oldnode <- nodeseq[length(nodeseq)-3] # at the opposite corner of last couple
			#senderPackets <- subset(all_stats,sender == oldnode & receiver %in% winners[winners!=closestCornerNode])
			#senderPackets <- senderPackets[order(-senderPackets[,constraint]),c("receiver")] # ordered
			#diagonalNode <- as.numeric(as.character(senderPackets[1]))


			diagonalNode <- getClosestIn(oldnode, all_stats, winners[winners!=closestCornerNode], constraint, 1)
			
			#Two opinions test:
			
		if(FALSE){			
			ccnWinners <- getWinners(closestCornerNode, all_stats, nodeseq, constraint, numWinners=2)
			oldnodeWinners <- getWinners(oldnode, all_stats, nodeseq, constraint, numWinners=2)
			diagonalWinners <- c(ccnWinners,oldnodeWinners)
			diagonalWinnerFreq <- table(diagonalWinners)
			print(diagonalWinnerFreq)
			if (max(diagonalWinnerFreq) > 1)
			{
				diagonalNode <- as.numeric(names(which.max(diagonalWinnerFreq)))

			}else #ask the previous node only!
			{
				diagonalNode <- getClosestIn(oldnode, all_stats, winners[winners!=closestCornerNode], constraint, 1)	
			}
			
			# Bu da ccn'in en yakin ikinci nodu secmesi.
			diagonalNode <- getWinners(closestCornerNode, all_stats, nodeseq, constraint,2)
			print(diagonalNode)
			diagonalNode <- diagonalNode[2]

#			stop("nodeSeq=",nodeseq," <= ADD diagonalNode:",diagonalNode)
			myprint("nodeSeq=",nodeseq," <= ADD diagonalNode:",diagonalNode,level=2,debugLevel=topDebugLevel)
		}
			
			# Two opinions test up to here!
			
			nodeseq <- c(nodeseq, diagonalNode) #add
			myprint(nodeseq,level=0,debugLevel=topDebugLevel)
			
			#Fix previous couple
			if (length(nodeseq) > 4 && FALSE )
			{
				lastnode1 <- nodeseq[length(nodeseq)]
				lastnode2 <- nodeseq[length(nodeseq)-1]
				
				senderPackets <- subset(all_stats,(receiver %in% nodeseq) & (sender == lastnode1))
				lastnode1Winners <- head(senderPackets[order(-senderPackets[,constraint]),],2)[,c("receiver")]
				lastnode1Winners <- as.numeric(as.character(lastnode1Winners))
				
				senderPackets <- subset(all_stats,(receiver %in% nodeseq) & (sender == lastnode2))
				#senderPackets <- senderPackets[order(-senderPackets$max_rssi),c("receiver","max_rssi")] # ordered
				lastnode2Winners <- head(senderPackets[order(-senderPackets[,constraint]),],2)[,c("receiver")]
				lastnode2Winners <- as.numeric(as.character(lastnode2Winners))
				winners <- c(lastnode1Winners,lastnode2Winners)
				
				winnerFreq <- table(winners)
				
				if (max(winnerFreq) > 1)
				{
					lastClosestCornerNode <- as.numeric(names(which.max(winnerFreq)))
					
					if (!(lastClosestCornerNode %in% c(nodeseq[length(nodeseq)-2],nodeseq[length(nodeseq)-3])))
					{
						myprint("unreliable",level=3,debugLevel=topDebugLevel); # uh-oh.. unreliable
						myprint(lastClosestCornerNode,c(nodeseq[length(nodeseq)-2],nodeseq[length(nodeseq)-3]),level=3)
					}else					
					if (lastClosestCornerNode == nodeseq[length(nodeseq)-2])
						{myprint("check good!",level=3,debugLevel=topDebugLevel);}#then it is good
					else # swap!
					{
						myprint("SWAP",level=3,debugLevel=topDebugLevel)
						dummy <- nodeseq[length(nodeseq)-2]
						nodeseq[length(nodeseq)-2] <- nodeseq[length(nodeseq)-3]
						nodeseq[length(nodeseq)-3] <- dummy
					}

				}

				
			}
			
		} #last node
		if (8 %in% nodeseq) {
			myprint("weird found",level=2,debugLevel=topDebugLevel)
			break;
		}
		node1 <- nodeseq[length(nodeseq)]
		node2 <- nodeseq[length(nodeseq)-1]
		
	} #while true
	
	cat("result:", nodeseq,"\n")
	resultSeq <- matrix(nodeseq,nrow=2,ncol=length(twoBy7)/2, dimnames=list(c(1:2),c(1:7)))
	myprint(resultSeq,level=1,debugLevel=topDebugLevel)
	
	Error<- matrix(rep(0,nrow(Truth)*ncol(Truth)),nrow=nrow(Truth), ncol=ncol(Truth)) # Comparison matrix
	#A<- matrix(as.character(Truth == resultSeq),nrow=nrow(Truth), ncol=ncol(Truth)) # Comparison matrix
	#A[A=="TRUE"]<-""
	#A[A=="FALSE"]<-"F"
	
	for (r in 1:nrow(Truth))
		for(c in 1:ncol(Truth))
		{
			resultLoc <- which(resultSeq==Truth[r,c], arr.ind=TRUE)
			diffRow <- abs(resultLoc[1,"row"]-r)
			diffCol <- abs(resultLoc[1,"col"]-c)
			Error[r,c] <- sum(diffRow, diffCol)
		}
		
	for(i in resultSeq)
		{
			truthLoc <- which(Truth==i, arr.ind=TRUE)
			resultLoc <- which(resultSeq==i, arr.ind=TRUE)
			diffRow <- abs(resultLoc[1,"row"]-truthLoc[1,"row"])
			diffCol <- abs(resultLoc[1,"col"]-truthLoc[1,"col"])
			r <- resultLoc[1,"row"]; c <- resultLoc[1,"col"]
			Error[r,c] <- max(diffRow, diffCol)
		}
		
	A<- matrix(as.character(Error),nrow=nrow(Truth), ncol=ncol(Truth)) # Comparison matrix
	A[A=="0"] <- ""
	
	if (FALSE){
	rowno <- 1
	plot(colnames(resultSeq),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),ylim=c(-4,0),main=RUN)
	text(as.numeric(colnames(resultSeq)),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),as.character(resultSeq[rowno,]),pos=1, col="blue")
	text(as.numeric(colnames(resultSeq)),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),as.character(Truth[rowno,]),pos=3, col="black")
	text(as.numeric(colnames(resultSeq)),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),as.character(A[rowno,]),pos=2, col="red")
	par(new=T) 
	rowno <- 2
	plot(colnames(resultSeq),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),ylim=c(-4,0))
	text(as.numeric(colnames(resultSeq)),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),as.character(resultSeq[rowno,]),pos=1, col="blue")
	text(as.numeric(colnames(resultSeq)),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),as.character(Truth[rowno,]),pos=3, col="black")
	text(as.numeric(colnames(resultSeq)),-as.numeric(rep(rownames(resultSeq)[rowno],ncol(resultSeq))),as.character(A[rowno,]),pos=2, col="red")
	}

	myprint(file,level=1,debugLevel=topDebugLevel)
	myprint("Error=====>>>>>",Error,level=1,debugLevel=topDebugLevel)
	print("Error Matrix:"); print(Error) ; print(resultSeq)
	maxError <- max(Error)
	meanError <- mean(Error)
	myprint("Max Error:", maxError,level=1,debugLevel=topDebugLevel)
	myprint("Mean Error:", meanError,level=1,debugLevel=topDebugLevel)
	
	cMaxError <- c(cMaxError,maxError)
	cMeanError <- c(cMeanError,meanError)

	#readkey()

} # For RUN
myprint ("RUNS:",runBegin,":",runEnd,"with",constraint,level=2,debugLevel=topDebugLevel)

myprint("Average maxError:",mean(cMaxError),level=2,debugLevel=topDebugLevel)
myprint("Average meanError:", mean(cMeanError),level=2,debugLevel=topDebugLevel)

myprint("Minimum maxError:",min(cMaxError),level=2,debugLevel=topDebugLevel)
myprint("Maximum maxError:",max(cMaxError),level=2,debugLevel=topDebugLevel)

table(cMaxError)

#table(a)

#rm(list=ls())
if (length(nodeseq) > 0 && FALSE)
			{
				prevnode1 <- nodeseq[length(nodeseq)-2]
				prevnode2 <- nodeseq[length(nodeseq)-3]
				fixingWinners <-c()
				senderPackets <- subset(all_stats,(receiver %in% c(node1,node2)) & (sender == closestCornerNode))
				ccnWinners <- head(senderPackets[order(-senderPackets[,constraint]),],2)[,c("receiver")]
				ccnWinners <- as.numeric(as.character(ccnWinners))
				
				senderPackets <- subset(all_stats,(receiver %in% c(node1,node2)) & (sender == diagonalNode))
				#senderPackets <- senderPackets[order(-senderPackets$max_rssi),c("receiver","max_rssi")] # ordered
				dnWinners <- head(senderPackets[order(-senderPackets[,constraint]),],2)[,c("receiver")]
				dnWinners <- as.numeric(as.character(dnWinners))
				
				fixingWinners <- c(ccnWinners,dnWinners)
				fixingWinnerFreq <- table(fixingWinners)
				if (max(winnerFreq) > 1)
				{
					prevNode <- as.numeric(names(which.max(fixingWinnerFreq)))
					if(nodeseq[length(nodeseq)-2] != prevNode) 
					{
						cat("fixPrev:", nodeseq[length(nodeseq)-2], nodeseq[length(nodeseq)-3],"\n")
							temp <- nodeseq[length(nodeseq)-2]
							nodeseq[length(nodeseq)-2] <- nodeseq[length(nodeseq)-3]
							nodeseq[length(nodeseq)-3] <- temp
						myprint(nodeseq,level=1,debugLevel=topDebugLevel)
					}
				}
			}