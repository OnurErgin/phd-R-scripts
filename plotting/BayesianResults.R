
removeSpaces <- function(...) gsub(" ","", ... , fixed=TRUE)


#> A
#     LeftToRight RightToLeft
#[1,]         100       93.95

data <- c(LeftToRight=100,RightToLeft=100)

mainTitle <- "Success With Bayesian Sequencing - Multiple Path Partitioning at d=4"
pdfFileName <- paste(removeSpaces(mainTitle),".pdf",sep="")
pngFileName <- paste(removeSpaces(mainTitle),".png",sep="")
mainTitle <- paste(mainTitle,"\nNorth Side")
pdf(pdfFileName)
A<-matrix(data,nrow=1,ncol=2,dimnames=list(NULL,names(data)), byrow=TRUE)
bp<-barplot(A,col=c("orange"),main=mainTitle,ylab="Success (%)")
text(bp, A, paste(A,"%",sep=""),cex=2,pos=1)
#text(bp, A, c("100%","~94%"),cex=2,pos=1)
dev.off()

# Convert pdf to png
system(paste("sips -s format png",pdfFileName,"--out",pngFileName))

"
Example 1 – Convert PDF to PNG
sips -s format png mypdf.pdf --out myimage.png
Example 2 – Resize based on width restriction
sips --resampleWidth 64 myimage.png --out myimage-resized.png
Example 3 – Convert and Resize
sips -s format png --resampleWidth 64 mypdf.pdf --out myimage-resized.png
"