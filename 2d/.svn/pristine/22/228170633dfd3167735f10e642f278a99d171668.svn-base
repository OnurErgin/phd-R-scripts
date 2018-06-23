#requires expID defined.
# expID must be between 1 and length(noise_levels)
nPackets <- 40

noise_levels <- seq(from=-20,to=-80,by=-5) 


  files <- Sys.glob(paste("inputs_simulation/noise_diversity/2D-analyze-5x4-5mX10m-Prune2-Noise",noise_levels[expID],"dbm.txt",sep=""))
  prune <- "2"; sd_rayleigh<-15; Pn <- noise_levels[expID]#dbm 
  tag <- paste(sep="","Sim-Pn=",Pn,"-")
  Nx <- 5; Ny <- 4; dx <- 5; dy <- 10
  expName <- paste(sep="",tag,Nx,"x",Ny,"_",dx,"mX",dy,"m-","prune",prune)
  Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)
  TruthSequence <- c(0:4,9:5,10:14,19:15)
