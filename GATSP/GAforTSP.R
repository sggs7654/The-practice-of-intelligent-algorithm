N <- 200 #竞争个体总数
L <- 1000 #适应度换算倍率
matingRate <- 0.8 #交配比例
variationRate <- 0.01 #突变概率
t <- 100 #迭代次数上限


GA4TSP <- function(distant){
  cN <- length(distant)
  population <- list()
  for(i in (1:N)){
    x <- rPath(cN) 
    population <- c(population, list(x))
  }
  date <- c()
  for(i in (1:t)){
    population <- choice(population, Eval, distant)
    population <- mating(population)
    population <- variation(population)
    #for(i in population){print(i)}
    date <- Display(population, Eval, distant,date)
    plot(date, type = "l", ylab = "种群平均适应度", xlab = "迭代次数", main = "种群平均适应度在迭代过程中的变化")
  }
}

rPath <- function(n){
  x <- sample(1:n, n, replace=F)
  x[n+1] <- x[1]
  x
}

Eval <- function(s,distant){
  total <- 0
  for(i in (2:length(s))){
    total <- total + distant[[s[i]]][s[i-1]]
  }
  total = 1 / total * L
}

Display <- function(p,f,distant,date){
  Roulette <- list()
  max <- 0
  best <- 0
  for(i in p){
    curr <- f(i, distant)
    if(curr > max){
      min <- curr
      best <- i
    }
    Roulette <- c(Roulette, list(curr))
  }
  total <- 0
  for(i in Roulette){
    total = total + i
  }
  total <- total / length(p)
  print(total)
  print(best)
  date <- c(date,c(total))
}

choice <- function(p, f, distant){
  Roulette <- list()
  for(i in p){
    Roulette <- c(Roulette, list(f(i, distant)))
  }
  total <- 0
  for(i in Roulette){
    total = total + i
  }
  newPopulation <- list()
  for(i in(1:length(p))){
    deadline <- runif(1, 0, total)
    point <- 0
    for(i in (1:length(Roulette))){
      point <- point + Roulette[[i]]
      if(point > deadline){
        newPopulation <- c(newPopulation,list(p[[i]]))
        break
      }
    }
  }
  newPopulation
}

mating <- function(p){
  n <- N * matingRate
  if(n %% 2 == 1){
    n <- n - 1
  }
  matingSeq <- sample(1:N, n, replace = F)
  for(i in (1:(n/2))){
    a <- matingSeq[i]
    b <- matingSeq[n+1-i]
    newBorn <- exchange(p[[a]], p[[b]])
    p[[a]] = newBorn[[1]]
    p[[b]] = newBorn[[2]]
  }
  p
}

exchange <- function(a,b){#使用OX交叉算子
  sa <- a
  sb <- b
  start <- sample(2:(length(a)-3),1)
  end <- sample((start+1):(length(a)-1),1)
  temp <- c()
  curr <- 0
  for(i in (1:length(a))){
    if(curr > (end-start)){
      break
    }
    for(j in (start:end)){
      if(b[i] == a[j]){
        temp <- c(temp,c(b[i]))
        b[i] <- a[start + curr]
        curr <- curr + 1
        break
      }
    }
  }
  curr <- 1
  for(j in (start:end)){
    a[j] = temp[curr]
    curr <- curr + 1
  }
  if((a[1] != a[length(a)]) | (b[1] != b[length(b)])){
    list(sa,sb)
  }else{
    list(a,b)
  }
}

variation <- function(p){
  n <- N * variationRate
  variationSeq <- sample(1:N, n, replace = F)
  for(i in (1:n)){
    p[[variationSeq[i]]] <- variate(p[[variationSeq[i]]])
  }
  p
}

variate <- function(a){
  locate <- sample(1:(length(a)-1),1)
  locate <-1
  if(locate == 1){
    repeat{
      value <- sample(1:(length(a)-1),1)
      if(value != a[1]){
        break
      }
    }
    for(i in (1:length(a))){
      if(a[i] == value){
        a[i] <- a[1]
        a[1] <- value
        a[length(a)] <- value
        break
      }
    }
    a
  }else{
    repeat{
      value <- sample(1:(length(a)-1),1)
      if(value != a[1]){
        break
      }
    }
    for(i in (1:length(a))){
      if(a[i] == value){
        a[i] <- a[locate]
        a[locate] <- value
        break
      }
    }
    a
  }
}

#填入你的数据路径↓
d <- read.table("C:/Users/asus/Desktop/GATSP/d.txt", quote="\"", comment.char="")
GA4TSP(d)
