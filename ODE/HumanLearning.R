library(R.matlab)
library(igraph)
library(ergm)
library(openxlsx)
library(sna)

D1M1=readMat("./CollectedData/Day1M1.mat")$Day1M1
D1M2=readMat("./CollectedData/Day1M2.mat")$Day1M2
D2M1=readMat("./CollectedData/Day2M1.mat")$Day2M1
D2M2=readMat("./CollectedData/Day2M2.mat")$Day2M2
D3M1=readMat("./CollectedData/Day3M1.mat")$Day3M1
D3M2=readMat("./CollectedData/Day3M2.mat")$Day3M2
D4M1=readMat("./CollectedData/Day4M1.mat")$Day4A1
D4M2=readMat("./CollectedData/Day4M2.mat")$Day4A2
xlsxM1<-read.xlsx("./CollectedData/globaltotal0712.xlsx",colNames=FALSE)
xlsxM2<-read.xlsx("./CollectedData/locallytotal0712.xlsx",colNames=FALSE)
xlsxM1=as.matrix(xlsxM1)
xlsxM2=as.matrix(xlsxM2)
colnames(xlsxM1)=NULL
colnames(xlsxM2)=NULL

current_M1=rbind(D1M1,D2M1,D3M1,D4M1,xlsxM1)
current_M2=rbind(D1M2,D2M2,D3M2,D4M2,xlsxM2)
fulldata=rbind(current_M1,current_M2)
labels=c(rep(1,nrow(current_M1)),rep(0,nrow(current_M2)))
datamatrix=cbind(labels,fulldata)
colnames(datamatrix)=c("identify",1:100)

summary_statistic_matrix=data.frame(labels=labels)


Mylist_current_M1=list()
for (i in 1:nrow(current_M1)){
  Mylist_current_M1[[i]]=matrix(current_M1[i,],byrow=TRUE,nrow=10)
}

Mylist_current_M2=list()
for (i in 1:nrow(current_M2)){
  Mylist_current_M2[[i]]=matrix(current_M2[i,],byrow=TRUE,nrow=10)
}

My_full_list=c(Mylist_current_M1,Mylist_current_M2)

tcdiff=numeric(0)
twopath=numeric(0)
density=numeric(0)
mutual=numeric(0)
balance=numeric(0)
ctri=numeric(0)
cycle4=numeric(0)
intransitive=numeric(0)
nearsimmelian=numeric(0)
simmelian=numeric(0)
transitive=numeric(0)
for (i in 1:length(My_full_list)){
  stat=summary(My_full_list[[i]]~ttriple+ctriple+density+twopath+mutual+balance+cycle(4))
  tcdiff=c(tcdiff,(stat[1]-stat[2])/(stat[3]^3))
  twopath=c(twopath,stat[4]/(stat[3])^2)
  density=c(density,stat[3])
  mutual=c(mutual,stat[5]/stat[3])
  balance=c(balance,stat[6]/stat[3])
  ctri=c(ctri,stat[2]/(stat[3]^3))
  cycle4=c(cycle4,stat[7]/(stat[3]^4))
}
summary_statistic_matrix["tcdiff"]=tcdiff
summary_statistic_matrix["twopath"]=twopath
summary_statistic_matrix["density"]=density
summary_statistic_matrix["mutual"]=mutual
summary_statistic_matrix["balance"]=balance
summary_statistic_matrix["ctri"]=ctri
summary_statistic_matrix["cycle4"]=cycle4

summary(glm(labels~cycle4,family="binomial"),data=summary_statistic_matrix)














l1=length(Mylist_current_M1)
l2=length(Mylist_current_M2)

twopath=numeric(0)
for (i in 1:length(Mylist_current_M1)){
  stat=summary(Mylist_current_M1[[i]]~twopath+density)
  twopath=c(twopath,stat[1]/(stat[2]^2))
}

twopath_non_identify=numeric(0)
for (i in 1:length(Mylist_current_M2)){
  stat=summary(Mylist_current_M2[[i]]~twopath+density)
  twopath_non_identify=c(twopath_non_identify,stat[1]/(stat[2]^2))
}
wilcox.test(twopath,twopath_non_identify)


mutual=numeric(0)
for (i in 1:length(Mylist_current_M1)){
  stat=summary(Mylist_current_M1[[i]]~mutual+density)
  mutual=c(mutual,stat[1]/stat[2])
}

mutual_non_identify=numeric(0)
for (i in 1:length(Mylist_current_M2)){
  stat=summary(Mylist_current_M2[[i]]~mutual+density)
  mutual_non_identify=c(mutual_non_identify,stat[1]/(stat[2]))
}

balance=numeric(0)
for (i in 1:length(Mylist_current_M1)){
  stat=summary(Mylist_current_M1[[i]]~balance+density)
  balance=c(mutual,stat[1]/(stat[2])^3)
}

balance_non_identify=numeric(0)
for (i in 1:length(Mylist_current_M2)){
  stat=summary(Mylist_current_M2[[i]]~balance+density)
  balance_non_identify=c(balance_non_identify,stat[1]/(stat[2])^3)
}
  

summary(My_full_list[[1]]~transitiveweights)