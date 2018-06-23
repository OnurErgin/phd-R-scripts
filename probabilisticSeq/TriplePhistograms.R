source("configuration.R")

library("plyr")
library("reshape")
library("ggplot2")
library(scales)

colm2Seq <- function(colNo,verdict=TRUE) {
  if (colNo ==12) 
    paste(seq(1,10), collapse="-") 
  else 
    paste(seq(colNo-3,colNo-1), collapse="-")
}
titleGenerate <- function(suc) {
  paste(experimentName,
        "Histogram of\n",
        "Experiment with",suc*100,"% success\n",
        "Differences in Partial probabilities for\n", 
        "Subsequence", colm2Seq(colm))
}

outputDirectory <- "./"
experimentName <- c("3rdFloor","1stRow","Reverse","NoCluster")
PrFile <- paste(outputDirectory,"PsOfBestSeqs-cluster5.txt",sep="")
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

if (FALSE) # All subseqs in one, 3-page histograms
{
  #experimentName <- c("4thFloor","1stRow","Rev")
  experimentName <- paste(experimentName,collapse="")
  plotTypes <- c("-a"=1,"-b"=-1)#,"-c"=0)
  plotFileName <- c("Probability","Differences","In","Position", "Windows",paste(success*100,sep=""))
  filename=paste(paste(plotFileName,collapse=""))
  #pdf(file=paste(filename,"-",experimentName,".pdf",sep=""))
  for (i in 1:length(plotTypes)) #plot differences in position probabilities
  {
    #Plot differences
    goodOnes <- as.data.frame(DiffPrs)
    goodOnes <- subset(as.data.frame(DiffPrs),verdict == plotTypes[[i]])
    
    plotTitle <- c(plotFileName,names(plotTypes[i]))
    meltedGoodOnes <- melt(goodOnes[,-c(1:3)])
    names(meltedGoodOnes)[1] <- "PositionWindow"
    ggp <- ggplot(meltedGoodOnes, aes(x=value,fill=PositionWindow)) 
    ggp <- ggp + facet_wrap(~PositionWindow)#,scales="free_x")
    ggp <- ggp + geom_histogram() + ggtitle (paste(experimentName,"\nHistogram of Partial probabilities for\n", paste(plotTitle,collapse=" ")) )
    print(ggp)
    ggsave (filename=paste(paste(plotFileName,collapse=""),".pdf",sep=""),plot=ggp)
  }
  #dev.off()
}

experimentName <- paste(experimentName,collapse="")
plotTypes <- c("-a"=1,"-b"=-1)#,"-c"=0)
plotFileName <- c("Probability","Differences","In","Position", "Windows",paste(success*100,sep=""))
filename=paste(paste(plotFileName,collapse=""))

colmStart <- 11 #start of subsequences
goodOnes <- subset(as.data.frame(DiffPrs),verdict == plotTypes[[1]])
badOnes <- subset(as.data.frame(DiffPrs),verdict == plotTypes[[2]])
badOnes <- badOnes[badOnes[,1]<=330,]
plots<-list()
filename=paste(paste(plotFileName,collapse=""))
  #pdf(file=paste(filename,"-",experimentName,".pdf",sep=""))
  for (colm in colmStart)
  {
    side.by.side <- data.frame(Type=character(0),ProbDiff=numeric(0),stringsAsFactors=FALSE)
    
    for (r in 1:nrow(goodOnes))
      side.by.side <- rbind(side.by.side,data.frame(Type="Correct Verdict: 8-9-10",ProbDiff=goodOnes[r,colm],stringsAsFactors=FALSE))
    
    for (r in 1:nrow(badOnes))
      side.by.side <- rbind(side.by.side,data.frame(Type="Wrong Verdict: 8-10-9",ProbDiff=badOnes[r,colm],stringsAsFactors=FALSE))
    
    ggp <- ggplot(side.by.side, aes(x=ProbDiff,fill=Type)) 
    ggp <- ggp + facet_wrap(~Type,nrow=2,ncol=1)#,scales="free_x")
    ggp <- ggp + theme(legend.position="none")
    ggp <- ggp + geom_histogram() + ggtitle (titleGenerate(success) )
    #ggp <- ggp + scale_x_continuous(breaks = round(seq(min(side.by.side$ProbDiff), max(side.by.side$ProbDiff), by=0.5),1))
    ggp <- ggp + scale_x_continuous(breaks=pretty_breaks(10), name="Difference in Probability")
    ggp <- ggp + scale_y_continuous(name="Number of Experiments")
    print(ggp)
    #plots[[i-3]] <- ggp
    #do.call("grid.arrange", ggp)
  }
  #dev.off()

#ml <- do.call(marrangeGrob, c(plots, list(nrow = 1, ncol = 10)));
#print(ml)
