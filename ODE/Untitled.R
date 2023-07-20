library(R.matlab)
library(igraph)
library(ergm)
library(openxlsx)
library(sna)
library(glmnet)

normalize_intransitive=function(p){
  return ((p^2)*(1-p)*(7*p*((1-p)^2)+3*p^3+3*(1-p)^3))
}

normalize_nearsimmelian=function(p){
  return ((p^5)*(1-p))
}

sim_ties_norm=function(p){
  return (1-(1-p^4)^8)
}

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


for (i in 1:length(My_full_list)){
  stat=summary(My_full_list[[i]]~ttriple+ctriple+density+twopath+mutual+balance+cycle(4)+intransitive+nearsimmelian+simmelian)
  tcdiff=c(tcdiff,(stat[1]-stat[2])/(stat[3]^3))
  twopath=c(twopath,stat[4]/(stat[3])^2)
  density=c(density,stat[3])
  mutual=c(mutual,stat[5]/stat[3])
  balance=c(balance,stat[6]/stat[3])
  ctri=c(ctri,stat[2]/(stat[3]^3))
  cycle4=c(cycle4,stat[7]/(stat[3]^4))
  intransitive=c(intransitive,stat[8]/normalize_intransitive(stat[3]))
  nearsimmelian=c(nearsimmelian,stat[9]/normalize_nearsimmelian(stat[3]))
  simmelian=c(simmelian,stat[10]/(stat[3]^6))
  
}

idegree_square=numeric(0)
for (i in 1:length(My_full_list)){
  stat=summary(My_full_list[[i]]~idegree(1:10))
  den=summary(My_full_list[[i]]~density)
  ss=0
  for (j in 1:10){
  ss=ss+(j^2)*stat[j]
  }
  ss=ss/(den+8*(den^2))
  idegree_square=c(idegree_square,ss)
}

odegree_square=numeric(0)
for (i in 1:length(My_full_list)){
  stat=summary(My_full_list[[i]]~odegree(1:10))
  den=summary(My_full_list[[i]]~density)
  ss=0
  for (j in 1:10){
    ss=ss+(j^2)*stat[j]
  }
  ss=ss/(den+8*(den^2))
  odegree_square=c(odegree_square,ss)
}
istar2=numeric(0)
istar3=numeric(0)
ostar2=numeric(0)
ostar3=numeric(0)

for (i in 1:length(My_full_list)){
  stat=summary(My_full_list[[i]]~istar(2)+istar(3)+ostar(2)+ostar(3)+density)
  istar2=c(istar2,stat[1]/(stat[5]^2))
  ostar2=c(ostar2,stat[2]/(stat[5]^2))
  istar3=c(istar3,stat[3]/(stat[5]^3))
  ostar3=c(ostar3,stat[4]/(stat[5]^3))
}

simmelianties=numeric(0)
for (i in 1:length(My_full_list)){
  stat=summary(My_full_list[[i]]~simmelianties+density)
  simmelianties=c(simmelianties,stat[1]/sim_ties_norm(stat[2]))
}

summary_statistic_matrix["tcdiff"]=tcdiff
summary_statistic_matrix["twopath"]=twopath
summary_statistic_matrix["density"]=density
summary_statistic_matrix["mutual"]=mutual
summary_statistic_matrix["balance"]=balance
summary_statistic_matrix["ctri"]=ctri
summary_statistic_matrix["cycle4"]=cycle4
summary_statistic_matrix["idegreesquare"]=idegree_square
summary_statistic_matrix["odegreesquare"]=odegree_square
summary_statistic_matrix["diff_degree_pop"]=idegree_square-odegree_square
summary_statistic_matrix["sum_degree_pop"]=idegree_square+odegree_square
summary_statistic_matrix["intransitive"]=intransitive
summary_statistic_matrix["istar2"]=istar2
summary_statistic_matrix["istar3"]=istar3
summary_statistic_matrix["ostar2"]=ostar2
summary_statistic_matrix["ostar3"]=ostar3
summary_statistic_matrix["simmelianties"]=simmelianties
summary_statistic_matrix["nearsimmelian"]=nearsimmelian
summary_statistic_matrix["simmelian"]=simmelian


library(pROC)
AUC=numeric(0)
for (i in 1:100){
sample=sample(1:nrow(summary_statistic_matrix),size=nrow(summary_statistic_matrix)*7/10)
training=summary_statistic_matrix[sample,]
validation=summary_statistic_matrix[-sample,]
model=glm(labels~tcdiff+twopath+density+mutual+balance+ctri+cycle4+diff_degree_pop+sum_degree_pop+intransitive+istar2+istar3+ostar2+ostar3+simmelianties+nearsimmelian+simmelian,family="binomial",data=training)

A=step(model,direction="both")

final_model=glm(formula = A$formula, family = "binomial", 
                data = training)


predpr <- predict(final_model,newdata=validation[,-1],type=c("response"))
validation$predpr=predpr
myROC=roc(validation$labels ~ validation$predpr)
AUC=c(AUC,as.numeric(myROC$auc))

}

AUC_2=numeric(0)
for (i in 1:100){
  sample=sample(1:nrow(summary_statistic_matrix),size=nrow(summary_statistic_matrix)*7/10)
  training=summary_statistic_matrix[sample,]
  validation=summary_statistic_matrix[-sample,]
  predictor=as.matrix(training[,-1])
  glmmod<-cv.glmnet(predictor,training$labels,family="binomial")
  
  predict(glmmod,newx=as.matrix(validation[,-1]),s=glmmod$lambda.min)
  
  glmmod_final=glmnet(predictor,training$labels,lambda=0.001246842,family="binomial")
  A<-predict(glmmod_final,newx=as.matrix(validation[,-1]))
  myROC=roc(validation$labels ~ as.numeric(A))
  AUC_2=c(AUC_2,as.numeric(myROC$auc))
}






