
source("configuration.R")
JPFile<-paste(outputDirectory,"JointP.txt",sep="")
TRFile <- paste(outputDirectory,"Trust.txt",sep="")
jointP <- read.table(JPFile,header=TRUE)
trusts <- read.table(TRFile,header=TRUE)
#correlationMatrix <- matrix(data = NA, nrow = length(files)*maxRankLevel, ncol = length(JointPcolumns), byrow = TRUE, dimnames = list(NULL,JointPcolumns))

correlations <- c()

for( i in 2:10)
{
  cor_i <- cor(jointP$verdict,jointP[,(3+i)], method="pearson") # As point-biserial correlation
  correlations<- c(correlations,cor_i)
  label <- paste("C_P",i,sep="")
  names(correlations)[i-1] <- label
}

P1P2diff_cor <- cor(trusts$verdict,trusts$NPdiff, method="pearson")
Pdiff_corDF <- as.data.frame(P1P2diff_cor); rm(P1P2diff_cor)

#titleText <- gsub("[A-Za-z0-9]*/","",getwd())
titleText <- gsub("/lhome/ergin/probabilisticSeq/","",getwd())
library("ggplot2")
correlationsDF <- as.data.frame(correlations) ;  rm(correlations)
p <- ggplot(correlationsDF, aes(x=rownames(correlationsDF),y=correlations,fill=correlations)) 
p <- p + geom_bar(position="dodge") +  scale_y_continuous(limits=c(-1,1),breaks=seq(-1, 1, 0.1)) #+ ylim(-1,1) #scale_y_continuous()
#p <- p + geom_bar(aes(x="Pdiff",y=Pdiff_corDF$P1P2diff_cor, color="P Difference"))
#p <- p + geom_line(x=rownames(correlationsDF),y=0.3)
p <- p + ggtitle(titleText)
ggsave (filename="correlations.jpg",plot=p)
print(p)
