Nx <- 10; Ny <- 5; dx <- 3; dy <- 3

Truth <- matrix (c(0:(Nx*Ny-1)), nrow=Ny, byrow=TRUE)

#Create C shape
#Truth[3:4,3:6] <- -1

# Nx <- 10  # Number of nodes in X-axis
# Ny <- 2 # Number of nodes in Y-axis
# dx <- 1 # internode distances on x-dimension, first tested value: 2
# dy <- 1.3 # internode distances on y-dimension, first tested value: 3

runSet <- 1#:100

# Radio parameters
Pt_dbm <- 0 
fc1 <- 2.405e9
spacing <- 0.005e9
channels <- (11:26)
ch1 <- channels[1]
ch2fc <- function (ch, firstChannel=ch1, firstFreq= fc1, ch_spacing=spacing) {return((ch-firstChannel)*ch_spacing + firstFreq)}
radioSensitivity <- -97
# Channel Settings
pathloss_alpha <- 3
sd_rayleigh <- 15 #org 10, 15 is very overlapping
mean_rayleigh <- 0

sd_noise_dbm <- -30
#sd_noise_dbm <- -30 #test : -30 daha iyi galiba. yakin mesafe icin
mean_noise <- 0

plotSamples <- TRUE
outputToFile <- FALSE
outputDirectory <- paste("./simOut-",Nx,"x",Ny,"-",dx,"mX",dy,"m-Noise",sd_noise_dbm,"dbm/",sep="")

if (!file.exists(outputDirectory) && outputToFile)
  dir.create(outputDirectory,showWarnings=TRUE,recursive=TRUE)


startTime <- proc.time()

for (runNo in runSet){
  run <- runNo
  source("2D-SimulatedNodes.R")
  
  # plotSamples <- TRUE
  if (plotSamples){
    a<-subset(as.data.frame(measurements),sender==0 & receiver %in% c(10,1,11,2,12)); a$receiver <- factor(a$receiver, levels=c(1,10,11,2,12))
    p <- ggplot (a,aes(x=channel,y=rssi,group=receiver)) + geom_boxplot() + facet_grid( . ~ receiver)
    #p <- ggplot (a,aes(x=channel,y=rssi,group=channel)) + geom_boxplot() + facet_grid( . ~ receiver)
    print(p)
  }
  
  if (outputToFile){
    filename <- paste(outputDirectory,"2dSim-",Nx,"x",Ny,"-",dx,"mX",dy,"m-",run,".txt",sep="")
    write.table(measurements, filename, col.names=TRUE, row.names=FALSE)
    if(run == 1){
      write.table(parameters, paste(outputDirectory,"parameters.txt",sep=""))
      write.table(Truth, paste( outputDirectory,"Truth.txt",sep=""))
    }
    
    rm("run")
    rm("measurements")
  }
}
## Print Elapsed Time
endTime <- proc.time()
print(endTime-startTime)