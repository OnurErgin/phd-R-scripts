library(reshape)
library(ggplot)
library(grid)
library(animation)

a_nodeseq <- as.matrix(read.table(text='"0"  "10" "1"  "11" "2"  "12" "3"  "13" "14" "15" "4"  "5"  "16" "6"  "7"  "17" "8"  "18" "9"  "19"'),nrow=2)
a_nodeseq <- c(0,10,1,11,2,12,3,13,14,15,4,5,16,6,7,17,8,18,9,19)
a_nodeseq <- matrix(a_nodeseq,nrow=2)
Truth <- matrix(0:19,nrow=2,ncol=length(twoBy10)/2, byrow=TRUE)


draw_path <- function (Truth,result){
  graph_input_truth <- melt(Truth, varnames=c("Y","X"))
  graph_input <- melt(result, varnames=c("Y","X"))
  graph_input_truth <- graph_input_truth[match(graph_input$value, graph_input_truth$value),]
  
  pl <- ggplot(graph_input_truth, aes(x=X,y=Y, label=value)) + geom_text() + scale_y_reverse()# + geom_path()
  for(i in 2:length(Truth) ){
    #pl <- ggplot(graph_input_truth, aes(x=X,y=Y, label=value)) + geom_text() + scale_y_reverse()# + geom_path()
    a <- graph_input_truth[(i-1):i,]
    pl <- pl + geom_path(data=a, aes(x=X,y=Y),arrow=arrow(), color="red") + geom_point(data=a, aes(x=X,y=Y), shape=1, size=6, color="red")
    print(pl)
    ani.pause()
  }
}

oopt = ani.options(interval = 0.3)
draw_path(Truth,winnerSeq) # display the animation 

saveHTML(draw_path(Truth,a_nodeseq), autoplay = FALSE, loop = FALSE, verbose = FALSE, outdir ="animated",#  paste(getwd(),"/animated/",sep=""),
         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")

