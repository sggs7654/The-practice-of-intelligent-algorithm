att48 <- read.table("C:/Users/asus/Desktop/57494203tsp-database/att48.tsp/att48.tsp", quote="\"", comment.char="")
View(att48)
n <- length(att48[[1]])
d <- list()
for(i in (1:n)){
  line <- c()
  for(j in (1:n)){
    if(j == i){
      line <- c(line,c(0))
    }else{
      x1 <- att48[[1]][i]
      x2 <- att48[[1]][j]
      y1 <- att48[[2]][i]
      y2 <- att48[[2]][j]
      distant <- sqrt((x1-x2)^2+(y1-y2)^2)
      line <- c(line,c(distant))
    }
  }
  d <- c(d,list(line))
}
