library(plyr)
library(reshape)
library(MASS)
library(reshape2)


d_from_rss <- function(pr, pt=0, fc=2.405e9, n=3, d0=1) {
  lamda <- 2.998e8/fc;
  pl = pt-pr
  d <- d0 * 10^((pl + 10*n*log10(lamda/(4*pi*d0))) / (10*n));
  return(d)
}

distanceTuples <- function (x, ref1, ref2)
{ #browser()
  all_receivers <- unique (x$receiver)
  distances <- data.frame(receiver = numeric(0), refnode1 = numeric(0), refnode2 = numeric(0))
  for (i in 1:length(all_receivers))
  {
    distances [i,1] <- all_receivers[i]
    distances [i,2] <- subset(x, receiver == all_receivers[i] & sender == ref1, select=c(distance))
    distances [i,3] <- subset(x, receiver == all_receivers[i] & sender == ref2, select=c(distance))
  }
  return(distances)
}

euclideanDist <- function (x1, x2, y1, y2) {sqrt( (x1-x2)^2 + (y1-y2)^2)}

successList <- c()

directory <- "inputs_simulation/equiDist/"
trainingDataSet <- 1:5

Nx=10; Ny=5; dx=3; dy=3; sd_noise_dbm <- -45
Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
refnodes <- c(Truth[1,1],Truth[Ny,1]) # Two corners

  startTime <- proc.time()

  DATA_FILE <- paste(directory,"distance_vector_stats_all_channels_",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm.csv.txt",sep="")
  cat("Loading:",DATA_FILE, "\n")  
  
  allstats <- read.csv (DATA_FILE, header=TRUE);
  experimentSet <- unique(allstats$run) ;

  training_stats <- subset(allstats, sender %in% refnodes & !(receiver %in% refnodes) & run %in% trainingDataSet, select=c(sender,receiver,avg_max_50Q,run))

  training_distances <- mutate(training_stats, distance=d_from_rss(pr=avg_max_50Q, n=3))
  training_tuples <- distanceTuples(training_distances,refnodes[1],refnodes[2])
      
  # This line averages all the training measurements
  training_tuples <- ddply(training_tuples,.(receiver), summarise, refnode1 = mean(refnode1), refnode2 = mean(refnode2))
  
for(i in experimentSet[-trainingDataSet])
{   
  measurement_stats <- subset(allstats, sender %in% refnodes & !(receiver %in% refnodes) & run == i, select=c(sender,receiver,avg_rssi,run)) 
  measured_distances <- mutate(measurement_stats, distance=d_from_rss(pr=avg_rssi, n=3))
  measured_tuples <- distanceTuples(measured_distances,refnodes[1],refnodes[2])
  
  closestPositions <- data.frame(receiver=as.numeric(measured_tuples$receiver),position=-1)
  
  for (rcvr in 1:nrow(measured_tuples)) 
  {
    #Find closest distance from training_distances to rcvr
    min_pos <- -1
    mindist <- 10000
    
    for (tr in 1:nrow(training_tuples))
    {
      if (training_tuples[tr,"receiver"] %in% closestPositions$position)
        next;
      #print(training_tuples[tr,"receiver"])
      
      eucdist <- euclideanDist(measured_tuples[rcvr,"refnode1"], training_tuples[tr,"refnode1"], measured_tuples[rcvr,"refnode2"], training_tuples[tr,"refnode2"])
      if (eucdist <= mindist)
      {
        min_pos <- training_tuples[tr,"receiver"]
        mindist <- eucdist
      }
    }
    closestPositions[rcvr, "receiver"] <- measured_tuples[rcvr, "receiver"]
    closestPositions[rcvr, "position"] <- min_pos
  }
  result <- Truth
  for (rr in 1:nrow(result))
  {
    for (cc in 1:ncol(result))
    {
      if (result[rr,cc] %in% refnodes)
      {  
        next;
      } else { 
        result[rr,cc] <- as.numeric(subset(closestPositions, receiver == result[rr,cc], select=c(position)))
      }
    }
  }
  cat("Simulation: ", i ,"\n")
  print(result)
  if (all(closestPositions$receiver == closestPositions$position))
  {
    successList <- c(successList,i)
  }
}

cat("Total Success: ", length(successList) / (length(experimentSet) - length(trainingDataSet)),"\n")
