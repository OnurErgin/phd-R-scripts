library(ggplot2)

source("../commonFunctions.R")

# sourceDir <- "../measurements/interferenceMeasurements/2D-NorthSide/controlledInterference2/"
# 
# expName <- strsplit(sourceDir,"/")[[1]][4]
# 
# expRange <- 103:122+600
# receiverid <- 90
# expPlace <- "4th Floor North"
# expName <- "rcv90_no-controlled-interferer"


# readExperimentsFromFile <- TRUE
# if (readExperimentsFromFile) {
#   packets1 <- readExpFileRange(sourceDir,expRange)
#   packets1 <- subset (packets1, receiver==receiverid)
# }

sourceDir <- "../measurements/node_health_check_measurements/0-148/"

expName <- strsplit(sourceDir,"/")[[1]][4]

expRange <- 1:5

readExperimentsFromFile <- TRUE
if (readExperimentsFromFile)
  packets1 <- readExpFileRange(sourceDir,expRange)


if(FALSE){
    p <- ggplot(packets1, aes(x=channel, y=rssi, group=channel))
    #p <- p + stat_boxplot(geom='errorbar') # for horizontal lines on boxplot borders
    #p <- p + geom_boxplot() #+ scale_y_continuous(breaks=c(min(packets$rssi):max(packets$rssi)))
    p <- p + geom_point(color="blue") + stat_summary(fun.y=mean, geom="line", aes(group=1))  + stat_summary(fun.y=mean, geom="point")
    p <- p + facet_wrap(~sender, scales="free_x")
    p <- p + scale_x_continuous(breaks=unique(packets1$channel))
    #p <- p + ggtitle(paste(expName,"\n",length(expRange),"runs, ",length(unique(packets$packetnum)),"packets/channel \n","Senders:")) + scale_fill_brewer(palette="PuOr", type="div")
    p <- p + ggtitle(paste(expName,"\n",expPlace,"\n","Receiver:",receiverid,"\n",length(expRange),"runs, ",length(unique(packets$packetnum)),"packets/channel \n","Senders:"))
    print(p)
} else 
  {
    p <- ggplot(packets1, aes(x=channel, y=rssi, fill=receiver))
    p <- p + stat_boxplot(geom='errorbar') # for horizontal lines on boxplot borders
    p <- p + geom_boxplot() #+ scale_y_continuous(breaks=-70:-20, limits=c(-70,-20)) # + scale_y_continuous(breaks=c(min(packets$rssi):max(packets$rssi)))
    p <- p + stat_boxplot(data=packets1, geom='errorbar') + geom_boxplot(data=packets1) 
    p <- p + facet_wrap(run~sender) 
    p <- p + scale_y_continuous(breaks=c(min(c(packets1$rssi,packets1$rssi)):max(c(packets1$rssi,packets1$rssi))))
    p <- p + scale_fill_brewer(palette="PuOr", type="div")
    p <- p + ggtitle ("0 to 11 and 148 at different times\n 5 runs each with 100 packets/channel\n Transmitter-Receiver 1meter apart\nSenders:")# + ggtitle(paste(expName,"\n",length(expRange),"runs, ",length(unique(packets$packetnum)),"packets/channel \n","Senders:"))
    print(p)
  }

a <- subset(packets1, channel%in%c(15) & run==1)
ggplot(a,aes(x=rssi, fill=channel)) + geom_histogram()# geom_histogram(position="dodge") + facet_wrap(~run)

d <- 1

rss_log_normal <- function(d, pt=0, fc=2.405e9, n=2, sigma=1, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -10*n*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pl <- pl + rnorm(1,sd=sigma);
  pt-pl
}
b<-c()
for (i in 1:100)
{
  b<-c(b,rss_log_normal(d))
}
b<-round(b)
b<- as.data.frame(b)
ggplot(b,aes(x=b)) + geom_histogram()

#ggsave(paste("plots/interference_",expName,".pdf",sep=""))

#ggsave(paste("plots/health_AllCh_Com_",expName,".pdf",sep=""))
