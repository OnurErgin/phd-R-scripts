library(ggplot2)
library (grid)
failedExperiments <- read.table("../temp/failSequences.txt", header=TRUE)

theme_update(axis.title.x = element_text(size = 12, vjust = -0.25),
             axis.title.y = element_text(size = 14, angle = 90, vjust = 1),
             axis.text.x = element_text(size = 11),
             axis.text.y = element_text(size = 11),
             panel.background = element_blank(),
             panel.grid.major = element_line(colour = "grey90"),
             panel.grid.major.y=element_blank(),
             panel.grid.major.x=element_blank(),
             legend.position = "right",
             legend.box = "horizontal",
             legend.key = element_blank(), 
             legend.background = element_rect(fill="white", size=0.25))

p <- ggplot(failedExperiments, aes(x=ExpNo,y=1)) + geom_bar(stat="identity", position="dodge", width=4)
p <- p + scale_y_discrete(name="Fail", limits=c(0,1.5),breaks=c(1)) + xlab("Experiment No")
p <- p + annotate("text",x=250, y=1.2, label= "20.12.2012 Thu 16:57\nto\n21.12.2012 Fri 17:48", color="red", size=3) +
         annotate("rect", xmin = 0, xmax = 500, ymin = 0.9, ymax = 1.4, alpha = 0.05) +
         annotate("text",x=1780, y=1.2, label= "26.12.2012 Thu 10:42\nto\n26.12.2012 Fri 23:41", color="red",size=3) +
         annotate("rect", xmin = 1500, xmax = 2040, ymin = 0.9, ymax = 1.4, alpha = 0.05) 
p <- p + annotate("text", x=1000, y=1.5, label="10 Nodes\n 93.55% Success", size=5)

print(p)

ggsave(filename="./plots/failedIndices.pdf", plot=p, width=6, height=5)


