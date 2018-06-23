
removeSpaces <- function(...) gsub(" ","", ... , fixed=TRUE)


if(FALSE){
	R <- matrix(rep(100,6),nrow=3,ncol=2,dimnames=list(c("Bayesian","MaxRSS", "MaxRss%{2,5,10}"),c("LeftToRight","RightToLeft")), byrow=TRUE)
	#bp<-barplot(R,beside=TRUE,col=c("lightblue", "mistyrose", "lavender"),main="Simulation",ylab="Success (%)",xlab="Methodology", names=rep(rownames(R),2))
	bp<-barplot(R,beside=TRUE,col=c("lightblue", "mistyrose", "lavender"),main="Simulation",ylab="Success (%)",xlab="Methodology") #, names=c("LeftEToRight","RightToLeft"))
	text(bp, y=0, labels=rep(rownames(R),2),cex=1,pos=4,srt=90)
	text(bp, R, labels=rep("100%",6),cex=1,pos=1,srt=0)
}

mainTitle <- "2D Position Discovery"
pdfFileName <- paste(removeSpaces(mainTitle),".pdf",sep="")
pngFileName <- paste(removeSpaces(mainTitle),".png",sep="")


# DF olmuyo bu, matrix olacak
data2D <- matrix(data = NA, nrow = 4, ncol = 2, byrow = TRUE, dimnames = list(c("1RefNode","2RefsParallel", "2RefsDiagonal", "2RefsTriangular"),c("Avg_maxError","Avg_meanError")))

data2D[1,] <- c(1.003,0.41)
data2D[2,] <- c(0.90,0.38)
data2D[3,] <- c(1.21,0.35)
data2D[4,] <- c(1.11,0.33)

pdf(pdfFileName)
bp <- barplot(data2D, beside=TRUE,col=c("blue","lightblue", "mistyrose", "lavender"),main=mainTitle,ylab="Error", ylim=c(0, 1.3))
text(bp, y=0, labels=rep(rownames(data2D),2),cex=1,pos=4,srt=90)
text(bp, data2D, labels=as.character(data2D),cex=1,pos=3,srt=0)
dev.off()


# Convert pdf to png
system(paste("sips -s format png",pdfFileName,"--out",pngFileName))
