library(plyr)
#library(ggplot2)
#source("configuration.R")
#source("commonFunctions.R")

P_STAT1 <- TRUE
P_STAT2 <- FALSE
Nx <- 10; Ny <- 2; dx <- 1; dy <- 1.3
#directory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m/",sep=""); setwd(directory);
directory <- "./"
output.file.name.p1 <- paste("distance_vector_stats_all_channels_",Nx,"x",Ny,"-",dx,"mX",dy,"m-",".csv.txt",sep="")
output.file.name.p2 <- "distance_vector_stats_per_channel_4thFl-North-Window-90Q.csv.txt"

experimentSet <- 1:2
startTime <- proc.time()
print(length(experimentSet))
for(expNo in experimentSet){ 
  TRACE_FILE <- paste(directory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
  cat(TRACE_FILE,"/", length(experimentSet), "\n")
  #packets <- read.table(TRACE_FILE, sep="\t", na.strings="", col.names=c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"), colClasses=c(rep("factor",3), "numeric", "factor", "character", "numeric"), header=FALSE)
  packets <- read.table(TRACE_FILE,  na.strings="", header=TRUE)
  #packets <- subset (packets, sender %in% Truth & receiver %in% Truth);
  packets <- droplevels(packets)
  if (P_STAT1) {
	  p_stat1<-ddply(packets, .(sender,receiver), summarise,
	                packets=length(rssi),
	                max_rssi=max(rssi), count_max_rssi=sum(rssi==max(rssi)),
#	                min_rssi=min(rssi),
#	                median_rssi=median(rssi),
	                avg_rssi=mean(rssi),
# 	                avg_ch11_rssi = mean(subset(rssi,channel==11)),
# 	                avg_ch18_rssi = mean(subset(rssi,channel==18)),
#   	              avg_ch26_rssi = mean(subset(rssi,channel==26)),
# 	                d_from_mean = d_from_rss(avg_rssi),
# 	                d_from_ch11 = d_from_rss(avg_ch11_rssi),
# 	                d_from_ch18 = d_from_rss(avg_ch18_rssi),
# 	                d_from_ch26 = d_from_rss(avg_ch26_rssi),
# 	                avg_mw=mean(dbm2mw(rssi)),
# 	                avg_ch11_mw = mean(dbm2mw(subset(rssi,channel==11))),
# 	                avg_ch18_mw = mean(dbm2mw(subset(rssi,channel==18))),
# 	                avg_ch26_mw = mean(dbm2mw(subset(rssi,channel==26))),
                  avg_rssi_1packet = mean(subset(rssi, packetnum == 1)),
#                   avg_ch11_rssi_1packet = mean(subset(rssi, packetnum == 1 & channel ==11)),
#                   avg_ch18_rssi_1packet = mean(subset(rssi, packetnum == 1 & channel ==18)),
#                   avg_ch26_rssi_1packet = mean(subset(rssi, packetnum == 1 & channel ==26)),
                  avg_rssi_8packet = mean(subset(rssi, packetnum %in% c(1:8) )),
#                   avg_ch11_rssi_8packet = mean(subset(rssi, packetnum %in% c(1:8) & channel ==11)),
#                   avg_ch18_rssi_8packet = mean(subset(rssi, packetnum %in% c(1:8) & channel ==18)),
#                   avg_ch26_rssi_8packet = mean(subset(rssi, packetnum %in% c(1:8) & channel ==26)),
                  avg_rssi_16packet = mean(subset(rssi, packetnum %in% c(1:16) )),
#                   avg_ch11_rssi_16packet = mean(subset(rssi, packetnum %in% c(1:16) & channel ==11)),
#                   avg_ch18_rssi_16packet = mean(subset(rssi, packetnum %in% c(1:16) & channel ==18)),
#                   avg_ch26_rssi_16packet = mean(subset(rssi, packetnum %in% c(1:16) & channel ==26)),
# 	                sd_rssi=sd(rssi),
# 	                avg_max_2=mean(subset(rssi, rssi>=quantile(rssi, 0.98))),
# 	                avg_max_5=mean(subset(rssi, rssi>=quantile(rssi, 0.95))),
# 	                avg_max_10=mean(subset(rssi, rssi>=quantile(rssi, 0.90))),
                 #AllCh_90Quantile = mean(subset(rssi, rssi>=quantile(rssi, 0.90)))
                    avg_max_98Q=mean(subset(rssi, rssi>=quantile(rssi, 0.98))),
                    avg_max_95Q=mean(subset(rssi, rssi>=quantile(rssi, 0.95))),
                    avg_max_90Q=mean(subset(rssi, rssi>=quantile(rssi, 0.90))),
                    avg_max_50Q=mean(subset(rssi, rssi>=quantile(rssi, 0.50)))
	                )
	  p_stat1$run<-expNo
	  
	  write.table(p_stat1, file=output.file.name.p1, sep=",", append=TRUE, col.names=(expNo==1), row.names=FALSE)
	  rm(p_stat1)
  }
  	
 if (P_STAT2) {
	  p_stat2<-ddply(packets, .(sender,receiver,channel), summarise,
	                packets=length(rssi),
	                max_rssi=max(rssi), count_max_rssi=sum(rssi==max(rssi)),
	                #min_rssi=min(rssi),
	                #median_rssi=median(rssi),
	                avg_rssi=mean(rssi),
	                sd_rssi=sd(rssi),
	                avg_max_98Q=mean(subset(rssi, rssi>=quantile(rssi, 0.98))),
	                avg_max_95Q=mean(subset(rssi, rssi>=quantile(rssi, 0.95))),
	                avg_max_90Q=mean(subset(rssi, rssi>=quantile(rssi, 0.90))),
	                avg_max_50Q=mean(subset(rssi, rssi>=quantile(rssi, 0.50)))
	                )
	  p_stat2$run<-i
	  write.table(p_stat2, file=output.file.name.p2, sep=",", append=TRUE, col.names=(i==1), row.names=FALSE)
	  rm(p_stat2)
  }
 
  rm(packets)
  gc(reset=TRUE)
  
}

## Print Elapsed Time
endTime <- proc.time()
print(endTime-startTime)
  
