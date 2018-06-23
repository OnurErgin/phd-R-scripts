removeSpaces <- function(...) gsub(" ","", ... , fixed=TRUE)

# Plot measurement values
if (TRUE) {
	mainTitle <- "Values from Real Measurements"
	pdfFileName <- paste(removeSpaces(mainTitle),".pdf",sep="")
	pngFileName <- paste(removeSpaces(mainTitle),".png",sep="")
	#mainTitle <- paste(mainTitle,"\nNorth Side")
	
	#Here comes plotting measurements
	Truth		<- c(16,138,96,141,92,145,88,147,10,153);
	TRACE_FILE<-"./seq16ch_15.txt"
		packets <- read.table(TRACE_FILE, sep="\t", na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("factor",3), "numeric", "factor", "character", "numeric"), header=FALSE)
		packets <- subset(packets,sender==16, select=c("receiver","rssi"))
		#levels(packets$receiver) <- as.character(Truth)
		packets$receiver <- factor(packets$receiver, levels=as.character(Truth))
		if (TRUE)
		{
			sortedDF <- as.data.frame(matrix(data = NA, nrow = 0, ncol = ncol(packets), byrow = TRUE, dimnames = list(NULL,colnames(packets))))
			for (i in Truth)
			{
				subpackets = subset(packets, receiver == i)
				sortedDF <- rbind(sortedDF,subpackets)
			}
			packets <- sortedDF
		}
	pdf(pdfFileName)
		#plot(packets$receiver,packets$rssi,pch=20, ylab="RSSI", xlab="Node Id (not distance sorted)"); title(mainTitle)
		plot(packets,pch=20, ylab="RSSI", xlab="Node Id (distance sorted)", ylim=c(-105,-42)); title(mainTitle)
		dev.off()
	
	
	# Convert pdf to png
	system(paste("sips -s format png",pdfFileName,"--out",pngFileName))
}
# Produce rayleigh distributed RSS values
if (FALSE)
{	
	source("parameters.R")
	
	test_distances <- seq(3,27,3)
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
	
	randData <- matrix(data = NA, nrow = 0, ncol = 2, byrow = TRUE, dimnames = list(NULL,c("Distance","RSS")))
	for (td in test_distances ) {
		A <- c()
		for (i in 1:shots) {
			h <- rayleigh_rnd(mean_rayleigh, sd_rayleigh)
			Pn <- rnorm(1, mean=0, sd=sd_noise_mW)
			P <- Pr_in_mW(Pt_mW, fc, td, pathloss_alpha, h, Pn)
			rssi <- mw2dbm(P)
			A <- c(A,rssi)	
			randData <- rbind(randData, c(td,rssi))
		}
		D<-c(D,mean((A)))
		C <- c(C,A)	
	}
	randData <- as.data.frame(randData)
	mainTitle <- "Produced values with Rayleigh Distribution"
	pdfFileName <- paste(removeSpaces(mainTitle),".pdf",sep="")
	pngFileName <- paste(removeSpaces(mainTitle),".png",sep="")
	
	if (TRUE)
	{
		pdf(pdfFileName)
		#plot(rep(test_distances,each=shots),C,pch=20, ylab="RSSI", xlab="Node Distance"); title("Produced values with Rayleigh")
		#plot(randData$Distance,randData$RSS,pch=20, ylab="RSSI", xlab="Node Distance"); title(mainTitle)
		bp <- boxplot(RSS ~ Distance, randData, ylab="RSSI", xlab="Node Distance"); title(mainTitle)
		abline(h=-97); text(bp,c(3,-97), labels="Sensitivity Treshold", pos=3)
		dev.off()
		
		# Convert pdf to png
		system(paste("sips -s format png",pdfFileName,"--out",pngFileName))
	}
}
