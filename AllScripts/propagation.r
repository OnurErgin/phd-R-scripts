library(plyr)
library(ggplot2)
library(grid)

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

set.seed(1234)


rss_path_loss <- function(d, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -20*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pt-pl
}

rss_log_normal <- function(d, pt=0, fc=2.405e9, n=2, sigma=1, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -20*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0);
  pl <- pl + rnorm(1,sd=sigma);
  pt-pl
}

d <- seq(1,50,by=0.5)

pr <-data.frame(distance=d,
                rss_pl_11=sapply(d, rss_path_loss, fc=2.405e9, n=3),
                rss_pl_26=sapply(d, rss_path_loss, fc=2.480e9, n=3),
                rss_pl_ln_11=sapply(d, rss_log_normal, fc=2.405e9, n=3, sigma=3),
                rss_pl_ln_26=sapply(d, rss_log_normal, fc=2.480e9, n=3, sigma=3)
                )

p <- ggplot(pr, aes(x=d))

p <- p + geom_line(aes(y=rss_pl_11, linetype="pl_11", shape="pl_11"), size=0.5)
p <- p + geom_line(aes(y=rss_pl_26, linetype="pl_26", shape="pl_26"), size=0.5)

p <- p + geom_point(aes(y=rss_pl_ln_11, shape="pl_ln_11", linetype="pl_ln_11"), size=1.5)
p <- p + geom_line(aes(y=rss_pl_ln_11,  shape="pl_ln_11", linetype="pl_ln_11"), size=0.25)
                 
p <- p + scale_x_continuous(limits=c(1,50))
p <- p + scale_y_continuous(limits=c(-100,-30))



p <- p + scale_linetype_manual(labels=c("fc=2.405 GHz", "fc=2.480 GHz", "sd=3 dB"), values=c(1,2,3), breaks=c("pl_11", "pl_26", NA), drop=FALSE, guide=guide_legend(title="log-distance (n=3, d0=1 m)", title.position="top", title.hjust=0, title.theme=element_text(size=8, angle=0), direction="vertical", keywidth=1, default.unit="cm",order=1))

p <- p + scale_shape_manual(labels=c("sd=3 dB"), values=c(0,0,1), breaks=c("pl_ln_11"), drop=FALSE, guide=guide_legend(title="shadowing", title.position="top", title.hjust=0, title.theme=element_text(size=8, angle=0), direction="vertical", keywidth=1, default.unit="cm", order=2))

p <- p + labs(x="Distance [m]", y="RSS [dBm]")

p

ggsave(p, file="log_normal_model_2_freq.pdf", width=12, height=8, scale=1.25, units="cm")

