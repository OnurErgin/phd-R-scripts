rss_path_loss <- function(d, pt=0, fc=2.405e9, n=2, d0=1) {
  lamda <- 2.998e8/fc;
  pl <- -10*n*log10(lamda/(4*pi*d0)) + 10*n*log10(d/d0); #fixed -20*log10( .... -> 10*n*log10( ...
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

#wrong
d_from_mw <- function (Pr, Pt = 1, fc=2.405e9, n=3) {
  lamda <- 2.998e8/fc;
  lamda/(4 * pi * ((Pr/Pt)^(1/n)))
}

mw2dbm <- function (milliW) { dBm <- 10*log10((milliW)); return(dBm) }
dbm2mw <- function (dBm)   { milliW <- 10^(dBm/10); return(milliW) }