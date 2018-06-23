source("configuration.R")

library("plyr")
library("reshape")
library("ggplot2")

outputDirectory <- "./"
experimentName <- c("4thFloor","2ndRow","Normal","5Cluster")
PrFile <- paste(outputDirectory,"PsOfBestSeqs-5cluster-4thFl2ndRow-Normal.txt",sep="")
Prs <- read.table(PrFile,header=TRUE)

success <- sum(Prs[Prs$verdict==1&Prs$rank==1,"verdict"])/length(unique(Prs$expNo))

shortPcolumnnames <- c()
for (i in 1:(numnodes-2))
{
  shortPcolumnnames <- c(shortPcolumnnames,paste(i,"-",i+2,sep=""))
}

Prcolumns <- c("expNo","verdict","rank",shortPcolumnnames,"1-10")
TriplePrs <- matrix(data = NA, nrow = nrow(Prs), ncol = length(Prcolumns), byrow = TRUE, dimnames = list(NULL,Prcolumns))

for (r in 1:nrow(Prs))
{
  TriplePrs[r,1:3] <- as.numeric(Prs[r,1:3])
  for (p in 1:(numnodes-2))
  {
    TriplePrs[r,3+p] <- prod(Prs[r,(3+p+1):(3+p+2)])
  }
  TriplePrs[r,"1-10"] <- prod(Prs[r,-c(1:3)])
}

# Compute differences in partial Pribabilities
DiffPrcolumns <- c("expNo","verdict","rank",shortPcolumnnames,"1-10")
DiffPrs <- matrix(data = NA, nrow = nrow(TriplePrs)/2, ncol = length(DiffPrcolumns), byrow = TRUE, dimnames = list(NULL,DiffPrcolumns))
for (r in seq(1,nrow(TriplePrs),2))
{
  if (TriplePrs[r,"expNo"] != TriplePrs[r+1,"expNo"])
  {
    stop("Different expNo at row",r,"and",r+1)
  }
  r1 <- TriplePrs[r,]
  r2 <- TriplePrs[r+1,]
  DiffPrs[(r+1)/2,1] <- r1[1]
  DiffPrs[(r+1)/2,-1] <- (r1-r2)[-1]
}

if (FALSE) #plot position Probabilities
{
  goodOnes <- subset(as.data.frame(TriplePrs),verdict == 0 & rank == 1)
  plotFileName <- c("Failed","Experiments","No","Clustering")
  
  meltedGoodOnes <- melt(goodOnes[,-c(1:3)])
  names(meltedGoodOnes)[1] <- "PositionWindow"
  ggp <- ggplot(meltedGoodOnes, aes(x=value,fill=PositionWindow)) 
  ggp <- ggp + facet_wrap(~PositionProbability)#,scales="free_x")
  ggp <- ggp + geom_histogram() + ggtitle (paste("Histogram of Partial probabilities for\n", paste(plotFileName,collapse=" ")) )
  print(ggp)
  #ggsave (filename=paste(paste(plotFileName,collapse=""),".pdf",sep=""),plot=ggp)
}

#experimentName <- c("4thFloor","1stRow","Rev")
experimentName <- paste(experimentName,collapse="")
plotTypes <- c("a"=1,"-b"=-1,"-c"=0)
plotFileName <- c("Probability","Differences","In","Position", "Windows",paste(success*100,sep=""))
filename=paste(paste(plotFileName,collapse=""))
pdf(file=paste(filename,"-",experimentName,".pdf",sep=""))
for (i in 1:length(plotTypes)) #plot differences in position probabilities
{
  #Plot differences
  goodOnes <- as.data.frame(DiffPrs)
  goodOnes <- subset(as.data.frame(DiffPrs),verdict == plotTypes[[i]])
  
  plotTitle <- c(plotFileName,names(plotTypes[i]))
  meltedGoodOnes <- melt(goodOnes[,-c(1:3)])
  names(meltedGoodOnes)[1] <- "PositionWindow"
  ggp <- ggplot(meltedGoodOnes, aes(x=value,fill=PositionWindow)) 
  ggp <- ggp + facet_wrap(~PositionWindow,scales="free_x")
  ggp <- ggp + geom_histogram() + ggtitle (paste(experimentName,"\nHistogram of Partial probabilities for\n", paste(plotTitle,collapse=" ")) )
  print(ggp)
  #ggsave (filename=paste(paste(plotFileName,collapse=""),".pdf",sep=""),plot=ggp)
}
dev.off()