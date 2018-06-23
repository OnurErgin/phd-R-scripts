library(reshape)
library(ggplot2)
library(RColorBrewer)

dataTypes <- c("NorthSide","Rayleigh", "NorthSide0int", "NorthSide3int", "NorthSide1int", "SouthSideWindow","SouthSideAisle-1packet", "SouthSideWindow-1packet", "SouthSideAisle-8packet", "SouthSideWindow-8packet")
dataType <- dataTypes[10]

if (dataType == "SouthSideWindow-8packet") #dataTypes[10]
{
  plotName <- "bayesianV1vsMDSvsGreedy-SouthSideWindow-8packet"
  experimentSize <- 2000
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    1373,1182,    # PNSD - Probabilistic Node Sequence Discovery
    0,575,    # MDS
    1109,883, # Greedy Avg
    446, 1647,    # Greedy AvgCh11    
    2, 0, #Greedy AvgCh18
    6,3         #Greedy AvgCh26
  ),
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}


if (dataType == "SouthSideAisle-8packet") # dataTypes[9]
{
  plotName <- "bayesianV1vsMDSvsGreedy-SouthSideAisle-8packet"
  experimentSize <- 2000
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    2000,1903,    # PNSD - Probabilistic Node Sequence Discovery
    395,2,    # MDS
    1994,213, # Greedy Avg
    54, 0,    # Greedy AvgCh11    
    1841, 1998, #Greedy AvgCh18
    0,0         #Greedy AvgCh26
  ),
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

if (dataType == "SouthSideWindow-1packet") #dataTypes[8]
{
  plotName <- "bayesianV1vsMDSvsGreedy-SouthSideWindow-1packet"
  experimentSize <- 2000
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    1464,1054,    # PNSD - Probabilistic Node Sequence Discovery
    0,257,    # MDS
    1105,854, # Greedy Avg
    598, 1673,    # Greedy AvgCh11    
    3, 0, #Greedy AvgCh18
    7,1         #Greedy AvgCh26
  ),
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

if (dataType == "SouthSideAisle-1packet")
{
  plotName <- "bayesianV1vsMDSvsGreedy-SouthSideAisle-1packet"
  experimentSize <- 2000
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    2000,1860,    # PNSD - Probabilistic Node Sequence Discovery
    1062,1,    # MDS
    1996,259, # Greedy Avg
    42, 0,    # Greedy AvgCh11    
    1848, 1997, #Greedy AvgCh18
    0,0         #Greedy AvgCh26
  ),
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

if (dataType == "SouthSideWindow")
{
  plotName <- "bayesianV1vsMDSvsGreedy-SouthWindow"
  experimentSize <- 2000
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    1201,1348,    # PNSD - Probabilistic Node Sequence Discovery
    0,871,    # MDS
    1124,883, # Greedy Avg
    402, 1642,    # Greedy AvgCh11    
    2, 1, #Greedy AvgCh18
    6,14       #Greedy AvgCh26
  ),
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

if (dataType == "NorthSide")
{  
  ###################################
  #North Side
  plotName <- "bayesianV1vsMDSvsGreedy-North"
  experimentSize <- 692
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
                      692,692,    # PNSD - Probabilistic Node Sequence Discovery
                      692,692,    # MDS
                      601,692, # Greedy Avg
                      65, 131,    # Greedy AvgCh11    
                      690, 573, #Greedy AvgCh18
                      25,25       #Greedy AvgCh26
                      ),
              ncol = 2, byrow = TRUE, 
              dimnames = list(dataNames,     # Rownames
                              c("LeftToRight", "RightToLeft"))                  # Colnames
            ) 
  #################################
}

# Cluster  vs no cluster
plotName <- "Set-A"
experimentSize <- 2000
dataNames <- c("no Pruning","m=5", "m=1");
results <- matrix(data = c(
  2000,1871,    # PNSD - Probabilistic Node Sequence Discovery without Clustering
  2000,1871,    # PNSD - Probabilistic Node Sequence Discovery with Pruning m=1
  2000,1871    # PNSD - Probabilistic Node Sequence Discovery with Pruning m=1
),
ncol = 2, byrow = TRUE, 
dimnames = list(dataNames,     # Rownames
                c("LeftToRight", "RightToLeft"))                  # Colnames
) 

if (dataType == "Rayleigh")
{
  # RAyleigh Simulation
  plotName <- "bayesianV1vsMDSvsGreedy-RayleighSim"
  experimentSize <- 2000
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
                            2000,2000,    # PNSD - Probabilistic Node Sequence Discovery
                            1737, 1702,    # MDS
                            2000,2000, # Greedy Avg
                            775, 637,    # Greedy AvgCh11    
                            750, 673, #Greedy AvgCh18
                            758, 639       #Greedy AvgCh26
                          ),
                      ncol = 2, byrow = TRUE, 
                      dimnames = list(dataNames,     # Rownames
                                      c("LeftToRight", "RightToLeft"))                  # Colnames
                    ) 

}

if (dataType == "NorthSide0int") {
  # RAyleigh Simulation
  plotName <- "NoInterference"
  experimentSize <- 300
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
                            300, 300,    # PNSD - Probabilistic Node Sequence Discovery
                            300, 300,    # MDS
                            300, 300, # Greedy Avg
                            300, 118,    # Greedy AvgCh11    
                            300, 289, #Greedy AvgCh18
                            300, 0       #Greedy AvgCh26
                          ),  
                    ncol = 2, byrow = TRUE, 
                    dimnames = list(dataNames,     # Rownames
                                    c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

if (dataType == "NorthSide3int") {
  # RAyleigh Simulation
  plotName <- "3 Interferers"
  experimentSize <- 300
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    299, 299,    # PNSD - Probabilistic Node Sequence Discovery
    299, 298,    # MDS
    299, 299, # Greedy Avg
    53, 40,    # Greedy AvgCh11    
    293, 299, #Greedy AvgCh18
    0, 0       #Greedy AvgCh26
  ),  
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

if (dataType == "NorthSide1int") {
  # RAyleigh Simulation
  plotName <- "1 Interferer"
  experimentSize <- 300
  dataNames <- c("PNSD","MDS","Greedy Avg", "Greedy AvgCh11", "Greedy AvgCh18", "Greedy Avg26");
  results <- matrix(data = c(
    300, 300,    # PNSD - Probabilistic Node Sequence Discovery
    300, 300,    # MDS
    300, 300, # Greedy Avg
    7, 97,    # Greedy AvgCh11    
    249, 300, #Greedy AvgCh18
    0, 0       #Greedy AvgCh26
  ),  
  ncol = 2, byrow = TRUE, 
  dimnames = list(dataNames,     # Rownames
                  c("LeftToRight", "RightToLeft"))                  # Colnames
  ) 
}

# Convert to percents:
results <- round((results/experimentSize) * 100,2)

results <- melt(results)

colnames(results)=c("algorithm","direction", "success")

#reorder bars
results <- within(results, algorithm <- factor(algorithm, levels = dataNames))

theme_update(axis.title.x = element_text(size = 12, vjust = -0.25),
             axis.title.y = element_text(size = 12, angle = 90, vjust = 0.25),
             axis.text.x = element_text(size = 11),
             axis.text.y = element_text(size = 11),
             panel.background = element_blank(),
             panel.grid.major = element_line(colour = "grey90"),
             legend.position = "right",
             legend.box = "horizontal",
             legend.key = element_blank(), 
             legend.background = element_rect(fill="white", size=0.25),
              legend.text = element_text(size=10))

p <- ggplot (results, aes(x=direction, y=success, fill=algorithm)) + geom_bar(stat="identity", position="dodge")
p<- p + geom_bar(stat="identity", position="dodge", colour="white", show_guide=FALSE) # to hide legend slashes
p <- p + labs(x="Direction", y="Success (%)")
p <- p + scale_fill_manual(name="PNSD Pruning", values = rev(brewer.pal(6,"YlGnBu")))#, guide=guide_legend(nrow=2,))
p <- p + geom_text(data=results, aes(y=(success),label=paste(success,"%",sep="")),position=position_dodge(width=0.9), size=4, vjust=2)
#p <- p + geom_text(data=results, aes(y=(50), label=algorithm),position=position_dodge(width=0.9), size=3.5, vjust=0, angle=90)
p <- p + ggtitle(plotName)
                           
print(p)
print(plotName)
ggsave(filename=paste("./plots/",plotName,"-pruning.pdf",sep=""), plot=p, width=6.86, height=5)
