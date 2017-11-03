input <- list(64, 22) #输入的温度，湿度
linear <- function(c,r) {
  k1 <- 1/r
  b1 <- -k1 * (c-r)
  k2 <- -(1/r)
  b2 <- -k2 * (c+r)
  f <- function(x){
    if(x < c){
      y <- k1*x + b1
    } else {
      y <- k2*x + b2
    }
    if(y < 0){
      y <- 0
    }
    y
  }
  f
}
F1 <- list(linear(0,40),linear(50,30),linear(100,40))
F2 <- list(linear(0,23.784),linear(30,15.01),linear(60,23.784))
iF <- list(F1, F2) #温度，湿度的隶属度函数
rule <- list( #推理规则：温度，湿度 -> 时间
  c(1,1,500),
  c(1,2,0),
  c(1,3,1000),
  c(2,1,1000),
  c(2,2,500),
  c(2,3,0),
  c(3,1,1000),
  c(3,2,500),
  c(3,3,500)
)

