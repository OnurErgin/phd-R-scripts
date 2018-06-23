#library(ggplot2)
#library(plyr)
#library(reshape)
#library(MASS)

#theme_set(theme_bw(8))
##
# Pr = ((|h|^2)/(d^a))*Pt + Pn
# Pr: Received Power (mW)
# Pt: Transmit Power (mW)
# Pn: Noise (mW) ~ Gaussian
# h: Rayleigh random variable h^2 = X^2 + Y^2, X,Y~G(0,sigma^2)
# d: Distance (m)
# a: Path loss alpha
# plot(rayleigh_rnd(1000,0,5),type='l')
# plot(density(rayleigh_rnd(1000,0,0.5)),type='l') #pdf
# plot.ecdf(rayleigh_rnd(1000,0,5)) #cdf

#P(dBm) = 10 · log10( P(mW) / 1mW )
#P(mW) = 1mW · 10(P(dBm) / 10)
#AWGN model with fading: additive with noise?

source("parameters.R")


MatrixRowSize <- length(channels)*length(nodes)*(length(nodes)-1)*numpackets

colnames<-c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum")

filePrefix <- "sim16ch_"
fileSuffix <- ".txt"

for(fileno in 1:2000)
{	
	packetsMatrix <- matrix(data = NA, nrow = MatrixRowSize, ncol = length(colnames), byrow = TRUE, dimnames = list(NULL,colnames))
	packetsMatrixIx <- 0
		
	
	for (ch in channels) 
	{
		fc <- ch2fc(ch)
		for (sender in nodes) 
		{
			for (receiver in subset(nodes, nodes!=sender)) 
			{
				h <- rayleigh_rnd(mean_rayleigh, sd_rayleigh)
				dist <- getDistance(sender,receiver)
				for (pkt in 1:numpackets ) 
				{
					Pn <- rnorm(1,mean_noise, sd_noise_mW)
					
					P_mw <- Pr_in_mW(Pt_mW, fc, dist, pathloss_alpha, h, Pn)
					P_dbm <- round(mw2dbm(P_mw))
					
					#cat("receiver=",receiver,"sender=",sender,"ch=",ch,"P=",P_dbm,"Pt_dbm=",Pt_dbm,"time=",0,"packetnum=",pkt,"\n")
					currentRow <- c(receiver,sender,ch,P_dbm,Pt_dbm,0,pkt)
					packetsMatrixIx <- packetsMatrixIx + 1	
					packetsMatrix[packetsMatrixIx,] <- currentRow
					
				}
			}
			
		}
	}
	
	
	filename <- paste(filePrefix,fileno,fileSuffix,sep="")
	write.table(packetsMatrix,file=filename,sep="\t", col.names=FALSE, row.names=FALSE)

	rm(packetsMatrix)
}






