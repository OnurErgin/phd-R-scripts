library(ggplot2)
library(gridExtra)

# data.set <- mds_node_coordinates$conf
#sequence<-names(sort(data.set[,1]))

getMatrix <- function(data.set, Nx=4, Ny=5, bycol=TRUE) {
  if (bycol) {
    clustNum <- Nx
  } else {
    clustNum <- Ny
  }
  
  fit <- kmeans(data.set[,1],clustNum) # cluster according to X-axis
  sequence <- names(sort(fit$cluster))
  matrixed.data <- matrix(as.numeric(sequence), byrow=FALSE, ncol=clustNum)
  print(matrixed.data)
  
  if (bycol){
    column.order <- match(matrixed.data[1,], # First row
                          names(sort(data.set[,1]))) # sort by X-axis
    result.matrix <- matrixed.data[,order(column.order)] # Sort Columns
  } else { # byrow! 
    row.order <- match(matrixed.data[,1], # First column
                       names(sort(data.set[,2]))) # Y-axis
    result.matrix <- matrixed.data[order(row.order),] # Sort Rows
  }
  
  print(result.matrix)
  
  # Sort columns within each other, bzw rows..
  if(bycol) { # sort columns within each other. 
    fixed.matrix <- result.matrix
    for (cc in 1:ncol(result.matrix)) {
      row.order <- match(result.matrix[,cc], # cc^th column
                         names(sort(-data.set[,2]))) # reverse sort by Y-axis
      fixed.matrix[,cc] <- result.matrix[order(row.order),cc] # Sort Columns
    }
  } else { # byrow
    
  }
  print (fixed.matrix)
}

# getMatrix(mds_node_coordinates$conf)

rc2xy <- function (r,c,dx, dy) {
  x <- (c-1)*dx
  y <- (r-1)*dy
  return(c(x=x,y=y))
}

rotate <- function(x) t(apply(x, 2, rev))
make.num.matrix <- function (x) { matrix(as.numeric(x),ncol=ncol(x))}

get.closest.point <- function (coords, scaled.data.set, exclude.list=NULL) {
  closest.dist <- max(scaled.data.set) * sqrt(2) * 2 # bigger than maximum possible distance in the grid
  closest.n <- 0
  for (n in 1:nrow(scaled.data.set)) {
    if (rownames(scaled.data.set)[n] %in% exclude.list){
      #cat(n, " is excluded\n")
      next;
    }
    d <- sqrt( (coords[1]-scaled.data.set[n,1])^2 + (coords[2]-scaled.data.set[n,2])^2 )
    if (d < closest.dist) {
      closest.dist <- d      
      closest.n <- n
    } # if       
  } # for
  #browser()
  return (rownames(scaled.data.set)[closest.n])
}

stretch.and.snap <- function (data.set, Nx=5, Ny=4, dx=2, dy=3) {
  length1 <- max(data.set[,1]) - min(data.set[,1]) # X-axis
  length2 <- max(data.set[,2]) - min(data.set[,2]) # Y-axis
  
  rangeX <- (Nx-1)*dx
  rangeY <- (Ny-1)*dy
  
  # rotation check, for long rectangular shape
#   if (length1 > length2 && rangeX > rangeY){
#     ; # no rotation
#   }else if ( length1 < length2 && rangeX < rangeY){
#     ; # no rotation
#   }else { # rotation
#     cat(" rotating ")
#     tempx <- dx; dx <- dy; dy <- tempx
#     tempx <- Nx; Nx <- Ny; Ny <- tempx
#     
#     rangeX <- (Nx-1)*dx
#     rangeY <- (Ny-1)*dy 
#   }
  
  scaled.data.set <- data.set
  for (n in 1:nrow(data.set)) {
    scaled.data.set[n,1] <- (data.set[n,1] - min(data.set[,1])) * (rangeX/length1)
    scaled.data.set[n,2] <- (data.set[n,2] - min(data.set[,2])) * (rangeY/length2)
  }
  
 # if (FALSE) #rotation check, for square shaped
  if (sum(scaled.data.set[,1] < dx) > Ny) { # Then it is rotated 90deg. rescale!
    tempx <- dx; dx <- dy; dy <- tempx
    tempx <- Nx; Nx <- Ny; Ny <- tempx
    
    rangeX <- (Nx-1)*dx
    rangeY <- (Ny-1)*dy
    
    scaled.data.set <- data.set
    for (n in 1:nrow(data.set)) {
      scaled.data.set[n,1] <- (data.set[n,1] - min(data.set[,1])) * (rangeX/length1)
      scaled.data.set[n,2] <- (data.set[n,2] - min(data.set[,2])) * (rangeY/length2)
    }
  }
  
  result <- matrix(data=rep(-1,Nx*Ny),ncol=Nx)
  #coords <- data.frame(); #rownames(coords) <- 0:(length(result)-1)
  for (r in 1:Ny) {
    for (c in 1:Nx) {
       # compute x,y coordinate of r,c
      coords <- rc2xy(r,c,dx,dy) 
      
       # Find the closest point in scaled.data.set
      closest.point <- get.closest.point(coords,scaled.data.set, exclude.list=result)
      
      result [r,c] = closest.point
    }
  }
  
  class(result) <- "integer"
#   class(Truth) <- "integer"
#   
#   if (all.equal(Truth,result) == TRUE) {
#     return (TRUE);
#   }
  
  p1 <- ggplot(as.data.frame(data.set), aes(x=D1,y=D2)) + geom_point(label=rownames(data.set)) + geom_text(label=rownames(data.set))
  p2 <- ggplot(as.data.frame(scaled.data.set), aes(x=D1,y=D2)) + geom_point() + geom_text(label=rownames(data.set))
  grid.arrange(p1,p2,ncol=1)
  return(result)
}

#stretch.and.snap(data.set, 5,4,2,3)
