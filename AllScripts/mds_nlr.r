library(plyr)
library(reshape)
library(MASS)

source("configuration.R")
source("commonFunctions.R")

  DATA_FILE <- "./inputs/distance_vector_stats_all_channels_4thFl-South-Aisle-nPacket.csv.txt"
#Truth <- 10:1; refnode <-10
allstats <- read.csv (DATA_FILE, header=TRUE);
experimentSet <- unique(allstats$run)

real_sequence<-as.character(Truth)

dv_type <- "avg_rssi"

mds_node_sequence<-vector("list", length(experimentSet))

#all_stats_rssi <- subset(allstats, select=c(run, sender, receiver, avg_rssi))
all_stats_rssi <- subset(allstats, (sender!=as.character(refnode) & receiver!=as.character(refnode)), select=c(run, sender, receiver, avg_rssi))
  real_sequence <- real_sequence[-1]

#all_stats_rssi_distance <- mutate(all_stats_rssi, distance=avg_rssi)
#all_stats_rssi_distance <- mutate(all_stats_rssi, distance=d_from_mw(Pr=avg_mw, n=3))
 all_stats_rssi_distance <- mutate(all_stats_rssi, distance=d_from_rss(pr=avg_rssi, n=3))

startTime <- proc.time()
print(length(experimentSet))

iteration <- 0
for (i in experimentSet) {
  iteration <- iteration + 1
  run_rssi_distance <- subset(all_stats_rssi_distance, run==i)

  run_rssi_distance_c <- cast(run_rssi_distance, receiver ~ sender, fill=24, value="distance")

  run_rssi_distance_d <-as.dist(run_rssi_distance_c)

   mds_node_coordinates<- cmdscale(run_rssi_distance_d,k=1)

  sequence<-names(sort(mds_node_coordinates[,1]))
  
  #cat("i=",i,all(sequence==real_sequence),"\n")
  
  mds_node_sequence[[iteration]]<-sequence
  if (sequence[length(sequence)]==real_sequence[[1]]) 
    mds_node_sequence[[iteration]]<-rev(sequence)
}

correct<-lapply(mds_node_sequence, identical, real_sequence)
length(correct[correct==TRUE])
  
binaryResults <- c()
for(i in 1:length(mds_node_sequence))
{
	binaryResults <- rbind(binaryResults,all(mds_node_sequence[[i]]==real_sequence))
}

## Print Elapsed Time
endTime <- proc.time()
print(endTime-startTime)

cat(paste("RefNode:",refnode,"Success:",sum(binaryResults)), "out of ", length(binaryResults),"experiments\n")

