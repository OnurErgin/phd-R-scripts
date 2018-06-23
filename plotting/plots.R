source("parameters.R")

test_distances <- seq(0,27,3)
sd_rayleigh <- 10
mean_rayleigh <- 0

sd_noise_dbm <- -45 
mean_noise <- 0

sd_noise_mW <- dbm2mw (sd_noise_dbm)
Pt_mW <- dbm2mw (Pt_dbm)

fc <- fc1

shots <- 40*16
#shots<-1
D <- c()
C <- c()
for (td in test_distances ) {
	A <- c()
	for (i in 1:shots) {
		h <- rayleigh_rnd(mean_rayleigh, sd_rayleigh)
		Pn <- rnorm(1, mean=0, sd=sd_noise_mW)
		#noise_sd_in_mW <- 0
		#Pn <- rnorm(1, mean=0, sd=noise_sd_in_mW)^2
		P <- Pr_in_mW(Pt_mW, fc, td, pathloss_alpha, h, Pn)
		#cat("P=",P,"\n")
		A <- c(A,mw2dbm(P))	
	}
	D<-c(D,mean((A)))
	C <- c(C,A)
}
if (FALSE) 
{
	
	i=4
	pdf(paste("rayleigh-",i,".pdf",sep=""))
	plot(test_distances,D,type="b",ylab="RSSI", xlab="Node Distance"); title("Computed single values with Rayleigh")
	dev.off()
	
}
if (TRUE)
{
	pdf("produced.pdf")
	plot(rep(test_distances,each=shots),C,pch=20, ylab="RSSI", xlab="Node Distance"); title("Produced values with Rayleigh")
	dev.off()
}

if (FALSE) {
	TRACE_FILE<-"~/PHD/R/FourthFloor_chLoop2000withPktID/measurements/seq16ch_15.txt"
	packets <- read.table(TRACE_FILE, sep="\t", na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("factor",3), "numeric", "factor", "character", "numeric"), header=FALSE)
	packets <- subset(packets,sender==16)
	nodes<-as.numeric(as.character(unique(packets$receiver)))
	pdf("measured.pdf")
	plot(packets$receiver,packets$rssi,pch=20, ylab="RSSI", xlab="Node Id (not distance sorted)"); title("Values from real measurements")
	dev.off()
}

mainTitle <- "Success With Bayesian Sequencing (without Clustering)"
pdf(paste(mainTitle,".pdf",sep=""))
A<-matrix(c(100,94),nrow=1,ncol=2,dimnames=list(NULL,c("LeftToRight","RightToLeft")), byrow=TRUE)
bp<-barplot(A,col=c("blue"),main=mainTitle,ylab="Success (%)")
#text(bp, A, paste(A,"%",sep=""),cex=2,pos=1)
text(bp, A, paste("~",A,"%",sep=""),cex=2,pos=1)
dev.off()

R <- matrix(rep(100,6),nrow=3,ncol=2,dimnames=list(c("Bayesian","MaxRSS", "MaxRss%{2,5,10}"),c("LeftToRight","RightToLeft")), byrow=TRUE)
#bp<-barplot(R,beside=TRUE,col=c("lightblue", "mistyrose", "lavender"),main="Simulation",ylab="Success (%)",xlab="Methodology", names=rep(rownames(R),2))
bp<-barplot(R,beside=TRUE,col=c("lightblue", "mistyrose", "lavender"),main="Simulation",ylab="Success (%)",xlab="Methodology") #, names=c("LeftEToRight","RightToLeft"))
text(bp, y=0, labels=rep(rownames(R),2),cex=1,pos=4,srt=90)
text(bp, R, labels=rep("100%",6),cex=1,pos=1,srt=0)

#> A
#     LeftToRight RightToLeft
#[1,]         100       93.95
