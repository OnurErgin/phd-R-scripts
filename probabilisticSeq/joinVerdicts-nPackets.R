library(ggplot2)
library(reshape)

library(RColorBrewer)


removeSpaces <- function(...) gsub(" ","", ... , fixed=TRUE)
removeSpaces <- function(...) {a<-gsub(" ","-", ... , fixed=TRUE); gsub(",","-", a , fixed=TRUE)}

# countVerify.R icinde expID'yi comment out et
setName <- c()
southAisleNormal <- c(10,18,22,26); setName[1] <- "South Aisle Normal"
southAisleReverse <- c(11,19,23,27); setName[2] <- "Set-A, Right-to-Left"

southWindowNormal <- c(2,20,24,28); setName[3] <- "South Window Normal"
southWindowReverse <- c(1,21,25,29); setName[4] <- "South Window Reverse"

SetA_Qx_Normal <- c(10,30,31,32); setName [5] <- "Set-A, Left-to-Right, Qx"
SetA_Qx_Reverse <- c(11,33,34,35); setName [6] <- "Set-A, Right-to-Left, Qx"

SetC_Qx_Normal <- c(2,36,37,38); setName [7] <- "Set-C, Left-to-Right, Qx"
SetC_Qx_Reverse <- c(1,39,40,41); setName [8] <- "Set-C, Right-to-Left, Qx"

sets <- SetC_Qx_Normal
sName <- setName[7]

nVerdicts <- data.frame()

for (set in sets){
  expID <- set
  source("countVerify.R")
  nVerdicts <- rbind(nVerdicts, finalVerdicts)
}

nVerdicts$numPackets <- factor(nVerdicts$numPackets, levels=sort(unique(nVerdicts$numPackets)))

nVerdicts$numPackets <- factor(nVerdicts$numPackets, levels=c("40","Q50","Q90", "Q100"))

p1 <- ggplot(nVerdicts, aes(x=reliability, fill=isCorrect))
p1 <- p1 + geom_bar() + scale_y_continuous("Number Of Experiments", limits=c(0,2000))
p1 <- p1 + scale_fill_manual2() + guides(fill=guide_legend(reverse=TRUE))
p1 <- p1 + ggtitle(paste(sName,"\n packets per Channel:")) + theme(panel.background=element_rect(fill="#FAFAFA"), plot.title=element_text(size=20), strip.text.x = element_text(size = 16), axis.text.x = element_text(size = 14, color = "black", angle = 45))
p1 <- p1 + scale_x_discrete(drop=FALSE)
p1 <- p1+facet_wrap(~numPackets, ncol=4)
#p1 <- p1 + stat_bin(aes(label=(..count..) ), vjust=-0.5, geom="text", position="identity") # add text above bars
print(p1)


# Time Lapse of nPacket comparison
fileName <- paste("verify/plots/3-levels-highExclusive/",removeSpaces(sName),"-nPackets.pdf", sep="")
#ggsave(fileName,p1)

#nVerdicts$reliability <- factor(nVerdicts$reliability, levels=c("high","medium", "low"))
p2 <- ggplot(nVerdicts, aes(x=expNo, y=numPackets, fill=reliability))
p2 <- p2 + geom_tile(width=1)
# Add plainFails:
#p2 <- p2 + geom_tile(data=plainFail, aes(x=expNo, y="plainFail", fill=isCorrect))
#p2 <- scale_y_discrete(limits=c("a","b","c","d"))
p2 <- p2 + scale_fill_brewer("Verdict", palette=5) + guides(fill=guide_legend(reverse=TRUE))
p2 <- p2 + ggtitle(sName) + theme(panel.background=element_rect(fill="#FAFAFA"), plot.title=element_text(size=10))
p2 <- p2 + scale_y_discrete(drop=FALSE)
print(p2)

fileName <- paste("verify/plots/3-levels-highExclusive/",removeSpaces(sName),"-nPackets-perExp.pdf", sep="")
#ggsave(fileName,p2)

dataNames <- c("1","8","16","40");
setA.nPacket <- matrix(data = c(
                                2000,1860,    # 1 Packet
                                2000,1903,    # 8
                                2000, 1905,   # 16
                                2000, 1871   # 40  
                              ),
                              ncol = 2, byrow = TRUE, 
                              dimnames = list(dataNames,     # Rownames
                                              c("LeftToRight", "RightToLeft"))                  # Colnames
                              ) 
sName <- "setA"

nPacketComparison <- melt(round((setA.nPacket/2000)*100,2))


setC.nPacket <- matrix(data = c(
                                1474,1054,    # 1 Packet
                                1373,1182,    # 8
                                1360, 1191,   # 16
                                1348, 1201   # 40  
                              ),
                              ncol = 2, byrow = TRUE, 
                              dimnames = list(dataNames,     # Rownames
                                              c("LeftToRight", "RightToLeft"))                  # Colnames
                              )
sName <-"setC"

nPacketComparison <- melt(round((setC.nPacket/2000)*100,2))

setC.MDSvsPNSD <- matrix(data = c(
  514,1054,    # 1 Packet
  1066,1182,    # 8
  1238, 1191,   # 16
  1450, 1201   # 40  
),
ncol = 2, byrow = TRUE, 
dimnames = list(dataNames,     # Rownames
                c("MDS", "PNSD"))                  # Colnames
)
sName <-"Set-C [MDS vs PNSD] RightToLeft"
nPacketComparison <- melt(round((setC.MDSvsPNSD/2000)*100,2))

# set-A normal
# 1p: 1984
# 8p:2000
# 16p:2000
# 40p:2000
# 
# setA.MDSvsPNSD <- matrix(data = c(
#   1984, 2000,    # 1 Packet
#   2000, 2000,    # 8
#   2000, 2000,   # 16
#   2000, 2000   # 40  
# ),
# ncol = 2, byrow = TRUE, 
# dimnames = list(dataNames,     # Rownames
#                 c("MDS", "PSDN"))                  # Colnames
# )
# sName <-"set-A -MDS vs PSND- RightToLeft"
# nPacketComparison <- melt(round((setA.MDSvsPNSD/2000)*100,2))

colnames(nPacketComparison) <- c("nPacket", "Algorithm", "success")
nPacketComparison$nPacket <- as.character(nPacketComparison$nPacket)
nPacketComparison$nPacket <- factor(nPacketComparison$nPacket, levels=dataNames)
nPacketComparison$Algorithm <- factor(nPacketComparison$Algorithm,levels=c("PNSD","MDS"))

theme_update(axis.title.x = element_text(size = 20, vjust = -0.25),
             axis.title.y = element_text(size = 20, angle = 90, vjust = 0.25),
             axis.text.x = element_text(size = 16, colour = "black"),
             axis.text.y = element_text(size = 16, colour = "black"),
             title = element_text(size=20),
             panel.background = element_blank(),
             panel.grid.major = element_line(colour = "grey90"),
             legend.position = "bottom",
             legend.box = "horizontal",
             legend.key = element_blank(), 
             legend.background = element_rect(fill="white", size=0.25),
             legend.text = element_text(size=18))

p3 <- ggplot(nPacketComparison, aes(x=nPacket, y=success, fill=Algorithm))
p3 <- p3 + geom_bar(stat="identity", position="dodge") 
#p3 <- p3 + scale_fill_brewer(palette="Paired") + guides(fill=guide_legend(reverse=FALSE))
p3 <- p3 + scale_fill_manual(name="Algorithm", values = c("#253494","#2C7FB8"))
p3 <- p3 + ggtitle(paste(sName)) + theme(panel.background=element_rect(fill="#FAFAFA"), legend.position="bottom")
p3 <- p3 + scale_x_discrete(name="Packets per Channel") + scale_y_continuous(name="Success [%]")
print(p3)
#p3 <- p3 + facet_wrap(~numPackets, ncol=4)
#p1 <- p1 + stat_bin(aes(label=(..count..) ), vjust=-0.5, geom="text", position="identity") # add text above bars

fileName <- paste("verify/plots/3-levels-highExclusive/",removeSpaces(sName),"-nPackets-NO-verify.pdf", sep="")
ggsave(fileName,p3)
