
## Functions

mw2dbm <- function (milliW) { dBm <- 10*log10((milliW)); return(dBm) }
dbm2mw <- function (dBm) 	{ milliW <- 10^(dBm/10); return(milliW) }

rayleigh_rnd <- function(mean=0,sd=1) { return(sqrt(rnorm(1,mean,sd)^2 + rnorm(1,mean,sd)^2)) }

Pr_in_mW <- function (Pt, fc, d, alpha, h, Pn) {
	lamda <- c/fc;
	Pr <- (h^2)*Pt*((lamda/(4*pi*d))^alpha) + Pn^2
	return(Pr)
}


c <- 2.998e8 #speed of light (m/s)

# Radio parameters
Pt_dbm <- 0 
fc1 <- 2.405e9
spacing <- 0.005e9
channels <- (11:26)
ch1 <- channels[1]
ch2fc <- function (ch, firstChannel=ch1, firstFreq= fc1, ch_spacing=spacing) {return((ch-firstChannel)*ch_spacing + firstFreq)}

# Node Settings
d <- 3 # internode distances
numnodes <- 10
nodes <- c(1:numnodes) # ids
refnode <- 1
distances <- seq(0,(numnodes-1)*d,d)
getDistance <- function(n1,n2) {abs(which(nodes==n1) - which(nodes==n2))*d}

numpackets <- 40

# Channel Settings
pathloss_alpha <- 3

sd_rayleigh <- 15
mean_rayleigh <- 0

sd_noise_dbm <- -100 
mean_noise <- 0

sd_noise_mW <- dbm2mw (sd_noise_dbm)
Pt_mW <- dbm2mw (Pt_dbm)

## Unused functions
rss_path_loss <- function(d, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -20*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pt-pl
}

d_path_loss <- function(pr, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl0 <- -20*log10(lamda/(4*pi*d0));
  d0*10^((pt-pr-pl0)/(10*n))
}
print ("Settings loaded.")