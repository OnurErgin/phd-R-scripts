## Test Edge Discoveries

source("EdgeDiscovery.R")

cols <- Nx <- 5; rows <- Ny <- 4; dx <- 3; dy <- 3; sd_noise_dbm <- -45

experimentSet <- 1:100 # 3x3fails: c(8, 16, 34, 54, 59, 83, 90) # 5x5fails: c(8, 34, 41, 59, 81, 83, 90)
Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
#refnodes <- c(Truth[1,1],Truth[Ny,1])
refnodes <- c(Truth[1,1],Truth[2,1])

numnodes <- length(Truth)
#directory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
#basedirectory <- "inputs_simulation/equiDist/3x3_ideal/"; subdirectory<-""
basedirectory <- "/Volumes/carme_lhome/R/2d/equiDist/"
subdirectory <- paste("simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")
directory <- paste(basedirectory,subdirectory,sep="")

pruneThreshold <- 2
source("probSeqFunctions-fixedPrune.R")

topQuantile <- 1
rssMaxDifference <- 0 

run.verbose <- FALSE
produceOutput <- FALSE # for probabilitySeqDF and probabilitiesDF. This file will produce output anyways

expSize <- length(experimentSet)

success.adjacents <- success.corners <- c()
selectedSuccess <- 0

selection_stats <- c(correct=0, total=0)

i <- 0
for(expNo in experimentSet)
{   
  startTime <- proc.time()
  i <- i + 1
  TRACE_FILE <- paste(directory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",expNo,".txt",sep="")
  cat("Loading:",TRACE_FILE, "\n")  
  packets <- read.table(TRACE_FILE,  na.strings="", header=TRUE)
  packets$time  <- 0
  packets$power <- 0
  
  TrueEdge <- Truth[1,] #Truth[1,1:4]
  
  refNodeSet1 <- discoverEdge(c(TrueEdge[1],TrueEdge[length(TrueEdge)]), packets, length(TrueEdge))  
  found <- all.equal(as.integer(refNodeSet1[1:length(TrueEdge)]),TrueEdge) == TRUE
  if (found) 
    success.corners <- c(success.corners,expNo) 
  cat("Corners:\t", refNodeSet1, " ",found, length(success.corners),"/",i,"\n")
  
  
  refNodeSet2 <- discoverEdge.adjacentRefNodes(c(TrueEdge[1],TrueEdge[2]), packets, length(TrueEdge))  
  found <- all.equal(as.integer(refNodeSet2[1:length(TrueEdge)]),TrueEdge) == TRUE
  if (found) 
    success.adjacents <- c(success.adjacents,expNo) 
  cat("Adjacents:\t", refNodeSet2, " ",found, length(success.adjacents),"/",i,"\n")

  if (refNodeSet1[length(TrueEdge)] > refNodeSet2[length(TrueEdge)+1]) {
    if (all.equal(as.integer(refNodeSet1[1:length(TrueEdge)]),TrueEdge) == TRUE)
      selectedSuccess <- selectedSuccess + 1
  } else {
    if (all.equal(as.integer(refNodeSet2[1:length(TrueEdge)]),TrueEdge) == TRUE)
      selectedSuccess <- selectedSuccess + 1
  }
  cat ("selectedSucces =", selectedSuccess,"\n")
  
}


cat("Success for CORNERS: ", length(success.corners), "\n")
cat("Success for ADJACENTS: ", length(success.adjacents), "\n")