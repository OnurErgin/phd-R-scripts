# Common functions:

mw2dbm <- function (milliW) { dBm <- 10*log10((milliW)); return(dBm) }
dbm2mw <- function (dBm) 	{ milliW <- 10^(dBm/10); return(milliW) }

rayleigh_rnd <- function(mean=0,sd=1) { return(sqrt(rnorm(1,mean,sd)^2 + rnorm(1,mean,sd)^2)) }


## Pr_in_mW
#  Pt 	: Transmit power 
#  fc 	: Frequency
#  d 	: Distance in meters
#  alpha: Path loss Alpha
#  h 	: Rayleigh random variable
#  Pn 	: White noise
##
Pr_in_mW <- function (Pt, fc, d, alpha, h, Pn) {
	c <- 2.998e8 #speed of light (m/s)
	lambda <- c/fc;
	Pr <- (h^2)*Pt*((lambda/(4*pi*d))^alpha) + Pn^2
	return(Pr)
}

## Pr_in_mW_obs : With uni-distance obstacles
#  Pt 	: Transmit power 
#  fc 	: Frequency
#  d 	: Distance in meters
#  alpha: Path loss Alpha
#  h 	: Rayleigh random variable
#  Pn 	: White noise
#  Wd	: inter-wall Distance
#  od 	: distance between obstacles
##
Pr_in_mW_obs <- function (Pt, fc, d, alpha, h, Pn, od) {
	c <- 2.998e8 #speed of light (m/s)
	lambda <- c/fc;
	Pr <- 0.9^(ceiling(d/od)-1)^2 * (h^2)*Pt*((lambda/(4*pi*d))^alpha) + Pn^2
	return(Pr)
}

## Unused functions
rss_path_loss <- function(d, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -10*n*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pt-pl
}

d_path_loss <- function(pr, pt=0, fc=2.405e9, n=3, d0=1) {
  lamda <- 2.998e8/fc;
  pl0 <- -10*n*log10(lamda/(4*pi*d0));
  d0*10^((pt-pr-pl0)/(10*n))
}

d_from_rss <- function(pr, pt=0, fc=2.405e9, n=3, d0=1) {
  lamda <- 2.998e8/fc;
  pl = pt-pr
  d <- d0 * 10^((pl + 10*n*log10(lamda/(4*pi*d0))) / (10*n));
  return(d)
}

colNameFix <- function (columnCount) 
{
  if (columnCount == 7)
    return (c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum"))
  else if (columnCount == 8)
    return (c("receiver", "sender", "channel", "rssi", "power", "time", "packetnum", "lqi"))
  else cat("Don't know what to do with ", columnCount, "column file\n");
}

colClassesFix <- function (columnCount) 
{
  if (columnCount == 7)
    return (c(rep("numeric",3), "numeric", "factor", "character", "numeric"))
  else if (columnCount == 8)
    return (c(rep("factor",3), "numeric", "factor", "character", "numeric", "numeric"))
  else cat("Don't know what to do with ", columnCount, "column file\n");
}

readExpFileRange <- function (sourceFolder, fileSet, prefix="seq16ch_") 
{
  # Test input
  tmp <- read.table(paste(sourceFolder,prefix,fileSet[1],".txt",sep=""),header=FALSE, nrows=2)
  nCols <- ncol(tmp)
  if (!(nCols %in% c(7,8)))
  {
    cat("Don't know what to do with ", nCols, "column file\n");
    return();
  }
  
  packets <- data.frame()
  for (i in fileSet)
  {
    TRACE_FILE<- paste(sourceFolder,prefix,i,".txt",sep="")
    expPackets <- read.table(TRACE_FILE, sep="\t", na.strings="", col.names=colNameFix(nCols), colClasses=colClassesFix(nCols), header=FALSE, stringsAsFactors=FALSE)
    expPackets$run <- i
    packets<-rbind(packets,expPackets)
  }
  return(packets)
}

#Compute 1D Position Error:
findError <- function(computedSequence, realSequence)
{
  fwError <- c()
  for (p in 1:length(realSequence))
  {
    location.in.computedSequence <- which(computedSequence == realSequence[p], arr.ind=FALSE)
    difference.in.location <- abs(location.in.computedSequence - p)
    fwError <- c(fwError, difference.in.location)
  }
  revError <- c()
  for (p in 1:length(realSequence))
  {
    location.in.computedSequence <- which(rev(computedSequence) == realSequence[p], arr.ind=FALSE)
    difference.in.location <- abs(location.in.computedSequence - p)
    revError <- c(revError, difference.in.location)
  }
  
  if (sum(fwError) < sum(revError))
    Error <- fwError
  else 
    Error <- revError
  return(Error)
}
