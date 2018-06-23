library(plyr)
library(ggplot2)
library(grid)
library(reshape)

source("commonFunctions.R")

theme_set(theme_bw(10))


theme_update(axis.text.x = element_text(size=10),
             axis.text.y = element_text(size=10),
             panel.grid.major = element_line(colour = "grey90"),
             legend.justification = c(1,1),
             legend.position = c(1,1),
             legend.box = "horizontal",
             legend.key = element_blank(),
             legend.background = element_rect(fill="white", size=0.25),
             panel.background = element_blank())

#set.seed(1234)


rss_path_loss <- function(d, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -10*n*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pt-pl
}

rss_log_normal <- function(d, pt=0, fc=2.405e9, n=2, sigma=1, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -10*n*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pl <- pl + rnorm(1,sd=sigma);
  pt-pl
}

Pr_in_mW <- function (Pt, fc,d, alpha, h, Pn) {
  c <- 2.998e8 #speed of light (m/s)
  lambda <- c/fc;
  Pr <- (h^2)*Pt*((lambda/(4*pi*d))^alpha) + Pn^2
  return(Pr)
}
d <- seq(1,10,by=0.1)

n<-3

mean_rayleigh<-0.71; sd_rayleigh<-1
mean_rayleigh<-0; sd_rayleigh<-15
sd_noise_dbm <- -45; sd_noise_mW <- dbm2mw (sd_noise_dbm)

awgn_simple <- c()
for (i in d)
{
  awgn_simple <- c(awgn_simple,mw2dbm(Pr_in_mW( Pt=dbm2mw(0), fc=2.405e9, d=i,alpha=n, h=1,Pn=0)))
}

awgn_h <- c()
for (i in d)
{
  awgn_h <- c(awgn_h,mw2dbm(Pr_in_mW( Pt=dbm2mw(0), fc=2.405e9, d=i,alpha=n, h=rayleigh_rnd(mean_rayleigh, sd_rayleigh),Pn=0)))
}

awgn_hPn <- c()
for (i in d)
{
  #awgn_hPn <- c(awgn_hPn,mw2dbm(Pr_in_mW( Pt=dbm2mw(0), fc=2.405e9, d=i,alpha=n, h=rayleigh_rnd(mean_rayleigh, sd_rayleigh),Pn=rnorm(1, mean=0, sd=sd_noise_mW))))
  awgn_hPn <- c(awgn_hPn,(rnorm(1, mean=0, sd=sd_noise_mW))^2)
}

pr <-data.frame(distance=d,
                rss_pl_11=sapply(d, rss_path_loss, fc=2.405e9, n=2),
                rss_pl_26=sapply(d, rss_path_loss, fc=2.480e9, n=2),
                rss_pl_ln_11=sapply(d, rss_log_normal, fc=2.405e9, n=2, sigma=3),
                rss_pl_ln_26=sapply(d, rss_log_normal, fc=2.480e9, n=2, sigma=3)
                )

pr$awgn_simple  <- awgn_simple
pr$awgn_h <- awgn_h
pr$awgn_hPn <- mw2dbm(dbm2mw(awgn_h) + awgn_hPn)

meltedPr <- melt.data.frame(pr,id="distance",measure=c("awgn_simple","awgn_hPn"))

p <- ggplot(meltedPr, aes(x=distance, y=value, color=variable))
p <- p + geom_line() + geom_point()
p <- p + scale_color_manual(name="Type of attenuation",
                             breaks=c("awgn_simple",  "awgn_hPn"),
                             labels=c("Ideal pathloss", "Simulated pathloss"),
                             values=c("black","blue"))

p <- p + labs(x="Distance [m]", y="RSS [dBm]") + theme(axis.title=element_text(size=20), text=element_text(size=18)) #+ theme(legend.position="none")

p <- p+ggtitle(paste("sd_rayleigh=", sd_rayleigh, " sd_noise_mW=", sd_noise_dbm, sep=" "))

print(p)
#ggsave("SimulationVsIdeal.pdf")
# p <- p + geom_line(aes(y=rss_pl_11, linetype="pl_11", shape="pl_11"), size=0.5)
# p <- p + geom_line(aes(y=rss_pl_26, linetype="pl_26", shape="pl_26"), size=0.5)
# 
# p <- p + geom_point(aes(y=rss_pl_ln_11, shape="pl_ln_11", linetype="pl_ln_11"), size=1.5)
# p <- p + geom_line(aes(y=rss_pl_ln_11,  shape="pl_ln_11", linetype="pl_ln_11"), size=0.25)
                 
p <- p + geom_point(aes(y=awgn_simple, shape="awgnsimp3", linetype="awgnsimp3"), size=1.5)
p <- p + geom_line(aes(y=awgn_simple,  shape="awgnsimp", linetype="awgnsimp"), size=0.25)

p <- p + geom_point(aes(y=awgn_h, shape="awgnh", linetype="awgnh"), size=1.5, color="red")
p <- p + geom_line(aes(y=awgn_h,  shape="awgnh", linetype="awgnh"), size=0.25, color="red")

p <- p + geom_point(aes(y=awgn_hPn, shape="awgnPn", linetype="awgnPn"), size=1.5, color="blue")
p <- p + geom_line(aes(y=awgn_hPn,  shape="awgnPn", linetype="awgnhPn"), size=0.25, color="blue")


# p <- p + scale_x_continuous(limits=c(1,50))
# p <- p + scale_y_continuous(limits=c(-100,-30))



#p <- p + scale_linetype_manual(labels=c("fc=2.405 GHz", "fc=2.480 GHz", "sd=3 dB", "ccc"), values=c(1,2,3,4), breaks=c("pl_11", "pl_26", NA, NA), drop=FALSE, guide=guide_legend(title="log-distance (n=3, d0=1 m)", title.position="top", title.hjust=0, title.theme=element_text(size=8, angle=0), direction="vertical", keywidth=1, default.unit="cm",order=1))

p <- p + scale_shape_manual(labels=c("sd=3 dB"), values=c(0,0,1), breaks=c("pl_ln_11"), drop=FALSE, guide=guide_legend(title="shadowing", title.position="top", title.hjust=0, title.theme=element_text(size=8, angle=0), direction="vertical", keywidth=1, default.unit="cm", order=2))

p <- p + labs(x="Distance [m]", y="RSS [dBm]") #+ theme(legend.position="none")

print(p)

#ggsave(p, file="Rayleigh_model_2_freq.pdf", width=12, height=8, scale=1.25, units="cm")

d <- seq(1,10,by=0.1)
RSSvsDistance <- function (d = seq(1,10,by=0.1), mean_rayleigh=0, sd_rayleigh=15, sd_noise_dbm = -45, pathloss_alpha = 3, constant_channel_gain = 23, saveToFile = FALSE) {
    
    sd_noise_mW <- dbm2mw (sd_noise_dbm)
    awgn_hPn <- c()
    awgn_simple <- c()
    for (i in d)
    {
      awgn_simple <- c(awgn_simple,mw2dbm(Pr_in_mW( Pt=dbm2mw(0), fc=2.405e9, d=i,alpha=pathloss_alpha, h=1,Pn=0))) 
    }
    
    awgn_simple <- awgn_simple + constant_channel_gain # add default channel gain for the ideal curve.. 
    
    for (i in d){
      Pn <- rnorm(1, mean=0, sd=sd_noise_mW) 
      h <- rayleigh_rnd(mean_rayleigh, sd_rayleigh)
      
      P <- Pr_in_mW(Pt_mW, ch2fc(ch), i, pathloss_alpha, h, Pn)
      rssi <- round(mw2dbm(P)) #- 5
      
      awgn_hPn <- c(awgn_hPn,rssi)
    }
    pr <-data.frame(distance=d, awgn_simple=awgn_simple, awgn_hPn=awgn_hPn)
    
    meltedPr <- melt.data.frame(pr,id="distance",measure=c("awgn_simple","awgn_hPn"))
    
    p <- ggplot(meltedPr, aes(x=distance, y=value, color=variable))
    p <- p + geom_line() + geom_point()
    p <- p + scale_color_manual(name="Type of attenuation",
                                breaks=c("awgn_simple",  "awgn_hPn"),
                                labels=c("Ideal pathloss", "Simulated pathloss"),
                                values=c("black","blue"))
    
    p <- p + labs(x="Distance [m]", y="RSS [dBm]") + theme(axis.title=element_text(size=20), text=element_text(size=18)) #+ theme(legend.position="none")
    
    p <- p+ggtitle(paste("sd_rayleigh=", sd_rayleigh, " sd_noise_mW=", sd_noise_dbm, "dbm", sep=" "))
    
    print(p)
    
    if(saveToFile)
      ggsave(paste("SimulationVsIdeal_sdRayleigh",sd_rayleigh,"_sdNoise",sd_noise_dbm,".pdf",sep=""))
    
  }
#RSSvsDistance (0,15,-30,3)
#RSSvsDistance (0,10,-45,3)
#RSSvsDistance (1/sqrt(2),0,-4599999999,3)
d <- seq(1,5,by=0.1)
RSSvsDistance (d, 0, 15,-45, 3, constant_channel_gain=23)
RSSvsDistance (d,mean_rayleigh= 1/sqrt(2), sd_rayleigh=0, sd_noise_dbm = -4599999999,pathloss_alpha=3, constant_channel_gain=0)
