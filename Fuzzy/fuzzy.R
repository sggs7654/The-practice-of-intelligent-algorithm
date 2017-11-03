fuzzy <- function(input,iF,rule){
  l = length(input) #特征数
  i = 1 #迭代当前处理的特征索引
  fuzzied = list() #保存模糊化后的输入，第一维是特征，第二维是一个list，包含其各个模糊描述的模糊度
  while(i <= l){#模糊化第i个特征
    temp <- list()
    for(f in iF[[i]]){ #对于第i个特征所对应的每个隶属度函数
      temp <- c(temp, list(f(input[[i]])))
    }
    fuzzied <- c(fuzzied, list(temp))
    i <- i + 1
  }
  activeRule <- list()#保存激活规则
  for(r in rule){ #遍历规则库
    d1 <- fuzzied[[1]][[r[[1]]]]
    d2 <- fuzzied[[2]][[r[[2]]]]
    if(d1>0 && d2>0){
      activeRule <- c(activeRule, list(list(r[[3]],min(d1,d2))))
    }
  }
  result <- list()#确定模糊输出
  for(a in activeRule){#模糊输出去重
    exist <- FALSE
    for(b in result){
      if(typeof(b) == "list" & b[[1]] == a[[1]]){
        exist <- TRUE
      }
    }
    if(!exist){
      result <- c(result, list(list(a[[1]],0)))
    }
  }
  for(i in (1:length(result))){#取隶属度最大的模糊输出
    for(a in activeRule){
      if(a[[1]] == result[[i]][[1]] & a[[2]] > result[[i]][[2]]){
        result[[i]][[2]] = a[[2]]
      }
    }
  }
  Swi <- 0
  Sw <- 0
  for(i in result){
    Swi <- Swi + i[[1]]*i[[2]]
    Sw <- Sw + i[[2]]
  }
  Swi / Sw
}




fuzzy(input,iF,rule)
