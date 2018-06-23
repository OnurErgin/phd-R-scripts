library(ggplot2)

source("../commonFunctions.R")
Truth  	<- c(16,138,96,141,92,145,88,147,10,153);
simTruth <- c(1:10);

measurementDir <- "../measurements/4thFloor/FourthFloor_chLoop2000withPktID/"

simulationDir <- "../measurements/simulation/"

expName <- strsplit(sourceDir,"/")[[1]][4]

expRange <- 2:3
mesPackets <- readExpFileRange(measurementDir,expRange)
simPackets <- readExpFileRange(simulationDir,expRange, prefix="sim16ch_")

Ns <- 1
Nr <- 2

mPackets <- subset(mesPackets, sender == Truth[Ns] & receiver%in%Truth[Nr])
sPackets <- subset(simPackets, sender == simTruth[Ns] & receiver%in%simTruth[Nr])

mPackets$type <- "measurement"
sPackets$type <- "simulation"
packets <- rbind(mPackets,sPackets)

p <- ggplot(packets, aes(x=channel, y=rssi))
p <- p + geom_point()
p <- p + facet_wrap(~type)

print(p)
