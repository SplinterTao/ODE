library(R.matlab)
library(igraph)
library(ergm)
D1M1=readMat("./CollectedData/Day1M1.mat")$Day1M1
D1M2=readMat("./CollectedData/Day1M2.mat")$Day1M2
D2M1=readMat("./CollectedData/Day2M1.mat")$Day2M1
D2M2=readMat("./CollectedData/Day2M2.mat")$Day2M2

current_M1=rbind(D1M1,D2M1)
current_M2=rbind(D1M2,D2M2)

Mylist_current_M1=list()
for (i in 1:nrow(current_M1)){
  Mylist_current_M1[[i]]=matrix(current_M1[i,],byrow=TRUE,nrow=10)
}
  
Mylist_current_M2=list()
for (i in 1:nrow(current_M2)){
  Mylist_current_M2[[i]]=matrix(current_M2[i,],byrow=TRUE,nrow=10)
}

