###
##
# This file keeps results from 2D , MDS-MAP and RADAR NN-Fingerprinting for comparisons. 
## R Studio Bug: if switch() cases start with a non-string '_', and you "source" it, then the R studio session fails.  eg switch (type,_5x4-3mx3m={...})

xnames <- c()
ix <- 0 # Assign indices below
xours <- ix<-ix+1; xSMDSMAPavgrssi <- ix<-ix+1; xSMDSMAPmaxrssi <- ix<-ix+1; xSMDSMAPavgrssi3edgeAnchor <- ix<-ix+1; xSMDSMAPavgrssi2edgeAnchor <- ix<-ix+1
xCMDSMAPavgrssi <- ix<-ix+1; xCMDSMAPmaxrssi <- ix<-ix+1; xFPavgrssi <- ix<-ix+1; xFPmaxrssi <- ix<-ix+1; 
#xnames <- c()
xnames[xours] <- "GBPD"; # Grid Based Position Discovery
xnames[xSMDSMAPavgrssi] <- "MDS-MAP"; # Smacof MDS-MAP with avg_rssi, Anchors 3; c(Truth[1,1],Truth[Ny,1],Truth[Ny,Nx])
xnames[xSMDSMAPmaxrssi] <- "sMDS-MAP (maxRSS)"; # Smacof MDS-MAP with max_rssi, "
xnames[xCMDSMAPavgrssi] <- "cMDS-MAP (avgRSS)" ; # Classic MDS-MAP with avg_rssi, "
xnames[xCMDSMAPmaxrssi] <- "cMDS-MAP (maxRSS)" ; # Classic MDS-MAP with max_rssi, "
xnames[xFPavgrssi] <- "Fingerprinting"; # Fingerprinting with avg_rssi, "
xnames[xFPmaxrssi] <- "Fingerprinting (maxRSS)"  # Fingerprinting with max_rssi, "
xnames[xSMDSMAPavgrssi3edgeAnchor] <- "sMDS-MAP (avgRSS-3EdgeAnchors)" # Smacof MDS-MAP with avg_rssi, Anchors Truth[c(1,2,Ny),1]
xnames[xSMDSMAPavgrssi2edgeAnchor] <- "sMDS-MAP (avgRSS-2EdgeAnchors)" # Smacof MDS-MAP with avg_rssi, Anchors Truth[c(1,Ny),1]

getResult <- function (topology,noise, refnodenum) {
  ## MDS-MAP and Fingerprinting Algorithms are given 3 anchors at 3 corners of the grid  <- c(Truth[1,1],Truth[Ny,1],Truth[Ny,Nx]) # Three corners
  noise <- as.character(noise)
  refnodenum <- as.character(refnodenum)
  #success[] Indices:
  
  success <- c()
  switch(refnodenum,
         "2" = {
                switch(noise,
                       "-45" = {                                ##### CLASSICAL max rssi MDS'lerin hepsini tekrar hesapla.
                                switch (topology,
                                  "5x4-3mx3m"={
                                    total <- 100
                                    success[xours] <- 90 # 2D-equiDistance
                                    success[xCMDSMAPavgrssi] <- 67 # Classic MDS-MAP with avg_rssi
                                    success[xCMDSMAPmaxrssi] <- 58 # Classic MDS-MAP with max_rssi
                                    success[xSMDSMAPavgrssi] <- 87 # Smacof MDS-MAP with avg_rssi, => NOT avg_max_50Q is used
                                    success[xSMDSMAPmaxrssi] <- 99 # Smacof MDS-MAP with max_rssi
                                    success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi
                                    success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                                    success[xSMDSMAPavgrssi3edgeAnchor] <- 87
                                    success[xSMDSMAPavgrssi2edgeAnchor] <- 87
                                    ## Reliability
                                    ours_success <- -1
                                    ours_fail    <- -1
                                    highReliableSuccess <- -1
                                    highReliableFail    <- -1
                                    mediumReliableSuccess <- -1
                                    mediumReliableFail    <- -1
                                    lowReliableSuccess    <- -1
                                    lowReliableFail       <- -1
                                  },
                                  
                                  "5x4-5mx5m"={
                                    total <- 100
                                    success[xours] <- 93 # 2D-equiDistance
                                    success[xCMDSMAPavgrssi] <- 72 # Classic MDS-MAP with avg_rssi
                                    success[xCMDSMAPmaxrssi] <- 64 # Classic MDS-MAP with max_rssi
                                    success[xSMDSMAPavgrssi] <- 100 # Smacof MDS-MAP with avg_rssi
                                    success[xSMDSMAPmaxrssi] <- 100 # Smacof MDS-MAP with max_rssi
                                    success[xFPavgrssi] <- 12 # Fingerprinting with avg_rssi
                                    success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                                  },
                                  "5x4-10mx10m"={
                                    total <- 100
                                    success[xours] <- 91 # 2D-equiDistance
                                    success[xCMDSMAPavgrssi] <- 87 # Classic MDS-MAP with avg_rssi
                                    success[xCMDSMAPmaxrssi] <- 57 # Classic MDS-MAP with max_rssi
                                    success[xSMDSMAPavgrssi] <- 100 # Smacof MDS-MAP with avg_rssi
                                    success[xSMDSMAPmaxrssi] <- 100 # Smacof MDS-MAP with max_rssi
                                    success[xFPavgrssi] <- 26 # Fingerprinting with avg_rssi
                                    success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                                  },
                                  "10x5-3mx3m"={
                                    total <- 100
                                    success[xours] <- 65 # 2D-equiDistance
                                    success[xCMDSMAPavgrssi] <- 13 # Classic MDS-MAP with avg_rssi
                                    success[xCMDSMAPmaxrssi] <- 2 # Classic MDS-MAP with max_rssi
                                    success[xSMDSMAPavgrssi] <- 69 # Smacof MDS-MAP with avg_rssi 28.7.15
                                    success[xSMDSMAPmaxrssi] <- 97 # Smacof MDS-MAP with max_rssi
                                    success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi 28.7.15
                                    success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                    success[xSMDSMAPavgrssi3edgeAnchor] <- 66
                                    success[xSMDSMAPavgrssi2edgeAnchor] <- 69
                                  },
                                  "10x2-3mx3m"={
                                    total <- 100
                                    success[xours] <- 97 # 2D-equiDistance
                                    success[xCMDSMAPavgrssi] <- 4 # Classic MDS-MAP with avg_rssi
                                    success[xCMDSMAPmaxrssi] <- 5 # Classic MDS-MAP with max_rssi
                                    success[xSMDSMAPavgrssi] <- 9 # Smacof MDS-MAP with avg_rssi
                                    success[xSMDSMAPmaxrssi] <- 9 # Smacof MDS-MAP with max_rssi
                                    success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi
                                    success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                    success[xSMDSMAPavgrssi3edgeAnchor] <- 1
                                    success[xSMDSMAPavgrssi2edgeAnchor] <- 0
                                  }
                                ) # Switch type
                         
                       }, 
                       "-30" = {
                         switch (topology,
                                 "5x4-3mx3m"={
                                   total <- 100
                                   success[xours] <- 89 #62 # 2D-equiDistance #adj.refs: 81
                                   success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                                   success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                                   success[xSMDSMAPavgrssi] <- 85 # Smacof MDS-MAP with avg_rssi
                                   success[xSMDSMAPmaxrssi] <- 9 # Smacof MDS-MAP with max_rssi
                                   success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi
                                   success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                   success[xSMDSMAPavgrssi3edgeAnchor] <- 84
                                   success[xSMDSMAPavgrssi2edgeAnchor] <- 85
                                   ## Reliability
                                   ours_success <- -1
                                   ours_fail    <- -1
                                   highReliableSuccess <- -1
                                   highReliableFail    <- -1
                                   mediumReliableSuccess <- -1
                                   mediumReliableFail    <- -1
                                   lowReliableSuccess    <- -1
                                   lowReliableFail       <- -1
                                 },
                                 
                                 "5x4-5mx5m"={
                                   total <- 100
                                   success[xours] <- 0 # 2D-equiDistance
                                   success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                                   success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                                   success[xSMDSMAPavgrssi] <- 98 # Smacof MDS-MAP with avg_rssi
                                   success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                                   success[xFPavgrssi] <- 14 # Fingerprinting with avg_rssi
                                   success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                 },
                                 "5x4-10mx10m"={
                                   total <- 100
                                   success[xours] <- 0 # 2D-equiDistance
                                   success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                                   success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                                   success[xSMDSMAPavgrssi] <- 84 # Smacof MDS-MAP with avg_rssi
                                   success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                                   success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi
                                   success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                 },
                                 "10x5-3mx3m"={
                                   total <- 100
                                   success[xours] <- 58 # 2D-equiDistance # high reliable: 0 28.7.15
                                   success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                                   success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                                   success[xSMDSMAPavgrssi] <- 0 # Smacof MDS-MAP with avg_rssi 28.7.15
                                   success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                                   success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi 28.7.15
                                   success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                   success[xSMDSMAPavgrssi3edgeAnchor] <- 0
                                   success[xSMDSMAPavgrssi2edgeAnchor] <- 0
                                 },
                                 "10x2-3mx3m"={
                                   total <- 100
                                   success[xours] <- 96 # 2D-equiDistance #95 with V2
                                   success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                                   success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                                   success[xSMDSMAPavgrssi] <- 0 # Smacof MDS-MAP with avg_rssi
                                   success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                                   success[xFPavgrssi] <- 2 # Fingerprinting with avg_rssi
                                   success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                                   success[xSMDSMAPavgrssi3edgeAnchor] <- 0
                                   success[xSMDSMAPavgrssi2edgeAnchor] <- 0
                                 }
                         ) # Switch type
                         
                       }
                ) #switch noise
              }, #2
         "3" = {
           switch(noise,
                  "-45" = {                                ##### CLASSICAL max rssi MDS'lerin hepsini tekrar hesapla.
                    switch (topology,
                            "5x4-3mx3m"={
                              total <- 100
                              success[xours] <- 95 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 67 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 58 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 72 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 99 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 5 # Fingerprinting with avg_rssi 29.7.15
                              success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                              success[xSMDSMAPavgrssi3edgeAnchor] <- 87
                              success[xSMDSMAPavgrssi2edgeAnchor] <- 87
                            },
                            
                            "5x4-5mx5m"={
                              total <- 100
                              success[xours] <- 93 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 72 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 64 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 100 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 100 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 12 # Fingerprinting with avg_rssi
                              success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                            },
                            "5x4-10mx10m"={
                              total <- 100
                              success[xours] <- 91 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 87 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 57 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 100 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 100 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 26 # Fingerprinting with avg_rssi
                              success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                            },
                            "10x5-3mx3m"={
                              total <- 100
                              success[xours] <- 75 # 2D-equiDistance 29.7.15
                              success[xCMDSMAPavgrssi] <- 13 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 2 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 28 # Smacof MDS-MAP with avg_rssi 28.7.15
                              success[xSMDSMAPmaxrssi] <- 97 # Smacof MDS-MAP with max_rssi 
                              success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi 29.7.15
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                              success[xSMDSMAPavgrssi3edgeAnchor] <- 66
                              success[xSMDSMAPavgrssi2edgeAnchor] <- 69
                            },
                            "10x2-3mx3m"={
                              total <- 100
                              success[xours] <- 97 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 4 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 5 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 9 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 9 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                              success[xSMDSMAPavgrssi3edgeAnchor] <- 1
                              success[xSMDSMAPavgrssi2edgeAnchor] <- 0
                            }
                    ) # Switch type
                    
                  }, 
                  "-30" = {
                    switch (topology,
                            "5x4-3mx3m"={
                              total <- 100
                              success[xours] <- 97 # 2D-equiDistance 
                              success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 85 # Smacof MDS-MAP with avg_rssi 29.7.15
                              success[xSMDSMAPmaxrssi] <- 9 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 3 # Fingerprinting with avg_rssi 29.7.15
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                              success[xSMDSMAPavgrssi3edgeAnchor] <- 84
                              success[xSMDSMAPavgrssi2edgeAnchor] <- 85
                            },
                            
                            "5x4-5mx5m"={
                              total <- 100
                              success[xours] <- 0 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 98 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 14 # Fingerprinting with avg_rssi
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                            },
                            "5x4-10mx10m"={
                              total <- 100
                              success[xours] <- 0 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 84 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                            },
                            "10x5-3mx3m"={
                              total <- 100
                              success[xours] <- 74 # 2D-equiDistance # high reliable: 0 29.7.15
                              success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 0 # Smacof MDS-MAP with avg_rssi 28.7.15
                              success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 0 # Fingerprinting with avg_rssi 29.7.15
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                              success[xSMDSMAPavgrssi3edgeAnchor] <- 0
                              success[xSMDSMAPavgrssi2edgeAnchor] <- 0
                            },
                            "10x2-3mx3m"={
                              total <- 100
                              success[xours] <- 92 # 2D-equiDistance
                              success[xCMDSMAPavgrssi] <- 0 # Classic MDS-MAP with avg_rssi
                              success[xCMDSMAPmaxrssi] <- 0 # Classic MDS-MAP with max_rssi
                              success[xSMDSMAPavgrssi] <- 0 # Smacof MDS-MAP with avg_rssi
                              success[xSMDSMAPmaxrssi] <- 0 # Smacof MDS-MAP with max_rssi
                              success[xFPavgrssi] <- 2 # Fingerprinting with avg_rssi
                              success[xFPmaxrssi] <- 0 # Fingerprinting with max_rssi
                              success[xSMDSMAPavgrssi3edgeAnchor] <- 0
                              success[xSMDSMAPavgrssi2edgeAnchor] <- 0
                            }
                    ) # Switch type
                    
                  }
           ) #switch noise
         } # 3
  ) #switch refnodenum 
  
  names(success) <- xnames
  print(success)
  return(success)
}

## Having 3 reference nodes at three corners: c(Truth[1,1],Truth[Ny,1],Truth[Ny,Nx])
getResult.3Refs <- function (topology,noise) {
  success <- c()
  switch(noise,
         "-45" = {                                
           switch (topology,
                   "5x4-3mx3m"={
                     total <- 100
                     success[xours] <- 20 # 2D-equiDistance
                     success[xCMDSMAPavgrssi] <- 67 # Classic MDS-MAP with avg_rssi
                     success[xCMDSMAPmaxrssi] <- 58 # Classic MDS-MAP with max_rssi
                     success[xSMDSMAPavgrssi] <- 100 # Smacof MDS-MAP with avg_rssi
                     success[xSMDSMAPmaxrssi] <- 99 # Smacof MDS-MAP with max_rssi
                     success[xFPavgrssi] <- 12 # Fingerprinting with avg_rssi
                     success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                     success[xSMDSMAPavgrssi3edgeAnchor] <- 87
                     success[xSMDSMAPavgrssi2edgeAnchor] <- 87
                   }
                )
           },
           "-30" = {
              switch(topology,
                     "5x4-3mx3m"={
                       total <- 100
                       success[xours] <- 20 # 2D-equiDistance
                       success[xCMDSMAPavgrssi] <- 67 # Classic MDS-MAP with avg_rssi
                       success[xCMDSMAPmaxrssi] <- 58 # Classic MDS-MAP with max_rssi
                       success[xSMDSMAPavgrssi] <- 100 # Smacof MDS-MAP with avg_rssi
                       success[xSMDSMAPmaxrssi] <- 99 # Smacof MDS-MAP with max_rssi
                       success[xFPavgrssi] <- 12 # Fingerprinting with avg_rssi
                       success[xFPmaxrssi] <- 1 # Fingerprinting with max_rssi
                       success[xSMDSMAPavgrssi3edgeAnchor] <- 87
                       success[xSMDSMAPavgrssi2edgeAnchor] <- 87
                     }
                    )
           }
         
      )
}


plotSuccess <- function (results, pl_name="", saveToFile=FALSE, select=NULL)
{
  detach("package:ggplot2", unload=TRUE)
  library(ggplot2)
  library(RColorBrewer)
  library(reshape)
  
  plotName <- pl_name# "Plot name"
  dataNames <- c("2d","MDS"); dimensionNames <- c("5x4", "10x2") 
  
  resultsDF <- results
  #resultsDF <- data.frame(name=names(results), success=results)
  
  #results <- melt(results)
  
  #reorder bars
    #resultsDF <- within(resultsDF, name <- factor(name, levels = names(results)))
  
  #results <- within(results, algorithm <- factor(algorithm, levels = dataNames))
  #results <- within(results, dimension <- factor(dimension, levels = dimensionNames))
  #browser()
  theme_update(axis.title.x = element_text(size = 20, vjust = -0.25),
               axis.title.y = element_text(size = 20, angle = 90, vjust = 0.25),
               axis.text.x = element_text(size = 12, color="black"),
               axis.text.y = element_text(size = 16, color="black"),
               title = element_text(size=20),
               panel.background = element_blank(),
               panel.grid.major = element_line(colour = "grey90"),
               panel.background = element_rect(fill=NA, color="black"),
               legend.position = "bottom",
               legend.box = "horizontal",
               legend.key = element_blank(), 
               legend.background = element_rect(fill="white", size=0.25),
               legend.text = element_text(size=14),
               strip.background = element_blank(),
               strip.text = element_text(size=14)
               )
  
  label_noise <- function (variable, value) {
    value <- as.character(value)
    if (variable=="noise") { 
      value[value=="-45"] <- "Moderate Noise"
      value[value=="-30"]   <- "High Noise"
    }
    return(value)
  }
  
  #  p <- ggplot (resultsDF, aes(x=noise, y=success, fill=name)) + geom_bar(stat="identity", position="dodge") # this groups x-axis with noise, and puts algorithms in bars
  p <- ggplot (resultsDF, aes(x=name, y=success, fill=name)) + geom_bar(stat="identity", position="dodge")+ facet_grid(. ~ noise, labeller = label_noise) # same as above, just simpler with facet.
  #p <- p + geom_bar(stat="identity", position="dodge", colour="white", show_guide=FALSE)  # to hide legend slashes
  p <- p + labs(x="White Noise Level") + scale_y_continuous("Success [%]", limits=c(0,100)) 
  p <- p + scale_x_discrete(label=c("-45"="Moderate", "-30"="High", "Fingerprinting" = "FP"))
  p <- p + scale_fill_manual(name="Algorithm", values = rev(brewer.pal(length(resultsDF$name),"YlGnBu"))[2:(length(resultsDF$name)-1)], guide=guide_legend(nrow=2))
  p <- p + geom_text(data=resultsDF, aes(y=(success),label=paste(success,"%",sep="")),position=position_dodge(width=0.9), size=5, vjust=0)
  #p <- p + geom_text(data=resultsDF, aes(y=(40),label=paste(name,"",sep="")),position=position_dodge(width=0.9), size=5, vjust=1.6, angle=90)
  #p <- p + geom_text(data=results, aes(y=(50), label=algorithm),position=position_dodge(width=0.9), size=3.5, vjust=0, angle=90)
  #p <- p + ggtitle(paste("Setting:",plotName))
  
  print(p)
  if (saveToFile)
  {
    plotfilename <- paste("./inputs_simulation/equiDist/plots/",plotName,"-",NumRefNodes,"refs.pdf",sep="")
    cat(plotName, "saving...",plotfilename,"\n")
    ggsave(filename=plotfilename, plot=p, width=6.86, height=5)
    
  }
}


e_type <- "10x5-3mx3m"
e_noise <- "-45"
NumRefNodes <- "3" ; xnames[xours] <- paste("GBPD-",NumRefNodes,sep="")
res <- getResult (e_type, e_noise, NumRefNodes)

if (e_noise == "-45") {
  e_noisename <- "ModerateNoise"
} else if (e_noise == "-30") 
  e_noisename <- "HighNoise"

selected_methods=c(xnames[xours],xnames[xSMDSMAPavgrssi],xnames[xFPavgrssi])

plot_inputDF <- data.frame(name=names(res), success=res,noise = e_noise, row.names=NULL)

e_noise <- "-30"; res <- getResult (e_type, e_noise, NumRefNodes)
plot_inputDF <- rbind (plot_inputDF, data.frame(name=names(res), success=res,noise = e_noise, row.names=NULL))

plot_inputDF <- subset(plot_inputDF, name %in% selected_methods)

plot_inputDF <- within(plot_inputDF, name <- factor(name, levels = selected_methods))

plotSuccess(plot_inputDF, paste(e_type), saveToFile=TRUE)

getReliability <- function (topology,noise){
  success <- c()
  switch(noise,
         "-45" = {
           switch (topology,
                   "5x4-3mx3m"={
                     success <- -1
                     fail    <- -1
                     highReliableSuccess <- -1
                     highReliableFail    <- -1
                     mediumReliableSuccess <- -1
                     mediumReliableFail    <- -1
                     lowReliableSuccess    <- -1
                     lowReliableFail       <- -1
                   }
           )
         }, 
         "-30" = {
           switch (topology,
                   "5x4-3mx3m"={
                     success <- -1
                     fail    <- -1
                     highReliableSuccess <- -1
                     highReliableFail    <- -1
                     mediumReliableSuccess <- -1
                     mediumReliableFail    <- -1
                     lowReliableSuccess    <- -1
                     lowReliableFail       <- -1
                   }
           )
         }
  )
}
