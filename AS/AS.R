#d <- read.table("C:/Users/asus/Desktop/R Project/AS/d7.txt", quote="\"", comment.char="")
#d <- list(c(0,3,1,2),c(3,0,5,4),c(1,5,0,2),c(2,4,2,0))
n <- length(d) #The number of cites
m <- 33 #The number of ants
alpha <- 1 #P weidget
bata <- 5 #d weidget
Rho <- 0.5
t <- 50

AS <-function(){
  P <- initPheromones()
  ave <- c()
  minist <- c()
  for(i in (1:t)){
    ants <- initAnts()
    ants <- buildPath(ants, P)
    #ants <- list(c(1,2,4,3,1),c(2,4,3,1,2),c(4,1,3,2,4))
    P <- updateP(ants,P)
    ave <-c(ave,c(Ave(ants)))
    minist <- c(minist,c(Min(ants)))
  }
  plot(ave, type = "l", col = 'blue', ylim = c(min(minist), max(ave)), ylab = "Path Length", xlab = "Iteration Times", main = "Blue: average  Red: minist")
  lines(minist, type = "l", col = 'red')
  print(min(minist))
  #for(ant in ants){print(ant)}
}

Min <- function(ants){
  LengthList <- c()
  for(ant in ants){
    antLength <- 0
    for(i in (1:(n-1))){
      antLength <- antLength + d[[ant[i]]][ant[i+1]]
    }
    antLength <- antLength + d[[ant[n]]][ant[1]]
    LengthList <- c(LengthList,c(antLength))
  }
  #print(min(LengthList))
  min(LengthList)
}

Ave <- function(ants){
  total <- 0
  for(ant in ants){
    antLength <- 0
    for(i in (1:(n-1))){
      antLength <- antLength + d[[ant[i]]][ant[i+1]]
    }
    total <- total + d[[ant[n]]][ant[1]]
    total <- total + antLength
  }
  total/m
}

updateP <- function(ants,P){
  Delta <- list()
  for(i in (1:n)){
    line <- c(0)
    for(j in (2:n)){
      line <- c(line,c(0))
    }
    Delta <- c(Delta, list(line))
  }
  for(ant in ants){
    PathLength <- 0
    for(i in (1:(length(ant) - 1))){
      PathLength <- PathLength + d[[ant[i]]][ant[i+1]]
    }
    for(i in (1:(length(ant) - 1))){
      Delta[[ant[i]]][ant[i+1]] <- Delta[[ant[i]]][ant[i+1]] + PathLength^-1
      Delta[[ant[i+1]]][ant[i]] <- Delta[[ant[i+1]]][ant[i]] + PathLength^-1
    }
  }
  for(i in (1:n)){
    for(j in (1:n)){
      P[[i]][j] <- P[[i]][j]*(1-Rho) + Delta[[i]][j]
    }
  }
  P
}

buildPath <- function(ants, P){
  repeat{
    for(i in (1:length(ants))){
      ants[[i]] <- nextCity(ants[[i]], P)
      if(length(ants[[i]]) == n){
        ants[[i]][n+1] <- ants[[i]][1]
      }
    }
    if(length(ants[[1]]) == n+1){
      break
    }
  }
  ants
}

nextCity <- function(ant, P){
  choice <- c()
  for(i in (1:n)){
    match <- FALSE
    for(j in ant){
      if(i == j){
        match <- TRUE
        break
      }
    }
    if(match){
      next
    }
    else{
      choice <- c(choice,c(i))
    }
  }
  Schoice <- c()
  for(i in choice){
    a <- P[[i]][ant[length(ant)]]^alpha
    b <- (1/d[[i]][ant[length(ant)]])^bata
    Schoice <- c(Schoice,c(a*b))
  }
  total <- 0
  for(i in Schoice){
    total <- total + i
  }
  line <- runif(1,0,total)
  point <- 0
  for(i in (1:length(Schoice))){
    point <- point + Schoice[i]
    if(point > line){
      ant <- c(ant,c(choice[i]))
      break
    }
  }
  ant
}

initAnts <- function(){
  ants <- list()
  ant <- c()
  for(i in (1:m)){
    start <- sample(1:n,1)
    ants <- c(ants,c(c(start)))
  }
  ants
}

initPheromones <- function(){
  start <- sample(1:n,1)
  first <- start
  repeat{
    NearestCity <- sample(1:n,1)
    if(NearestCity != start){break}
  }
  Path <- c(start)
  PathLength <- 0
  for(t in (2:n)){
    for(i in (1:n)){
      match <- FALSE
      for(j in Path){
        if(i == j){
          match <- TRUE
          break
        }
      }
      if(match){next}
      if((i != start) & (d[[start]][i] < d[[start]][NearestCity])){
        NearestCity <- i
      }
    }
    Path <- c(Path,c(NearestCity))
    PathLength <- PathLength + d[[start]][NearestCity]
    start <- NearestCity
    repeat{
      NearestCity <- sample(1:n,1)
      match <- FALSE
      for(j in Path){
        if(NearestCity == j){
          match <- TRUE
          break
        }
      }
      if((NearestCity != start) & (!match)){break}
      if(length(Path) == n){break}
    }
  }
  PathLength <- PathLength + d[[first]][Path[length(Path)]]
  Ave <- m / PathLength
  P <- list()
  for(i in (1:n)){
    temp <- c()
    for(j in (1:n)){
      if(j == i){
        temp <- c(temp,c(0))
      }else{
        temp <- c(temp,c(Ave))
      }
    }
    P <- c(P,list(temp))
  }
  P
}

AS()