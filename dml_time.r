library(BART)
library(BayesTree)
library(tidyverse)
library(caret)

#create record data frame:
dataset.num=rep(formatC(1:3400, width=4, flag = "0"),each=15)
variable=rep(c("Overall","Overall","Overall","X1","X1","X2","X2","X2","X3","X3","X4","X4","X4","X5","X5"),3400)
level=rep(c(NA,NA,NA,0,1,"A","B","C",0,1,"A","B","C",0,1),3400)
year=rep(c(NA,3,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),3400)
satt=vector(mode = "numeric", length = 51000)
lower90=vector(mode = "numeric", length = 51000)
upper90=vector(mode = "numeric", length = 51000)
df_record=data.frame(dataset.num,variable,level,year,satt,lower90,upper90)
write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/dml_time_record.csv",row.names = FALSE)   
count=0
K=5
alpha=0.1

for (i in 1:3400){
  path=str_interp("/Users/piglet/Desktop/track2_20220404/merged/acic_practice_${i}.csv")
  print(i)
  df=read.csv(path)
  df_record=read.csv("/Users/piglet/Desktop/track2_20220404/dml_time_record.csv")
  df_task1=df %>% dplyr::filter(year==2) %>% select(-year,-post,-p_score)
  df_task2=df %>% dplyr::filter(year %in% c(3,4)) %>% select(-post,-p_score)
  
  df_task1$y_1=(df %>% dplyr::filter(year==1))$Y
  df_task1$y_2=(df %>% dplyr::filter(year==2))$Y
  df_task1$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_task1$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_task1$size_2=(df %>% dplyr::filter(year==2))$n.patients
  
  df_task2$y_1=rep((df %>% dplyr::filter(year==1))$Y,each=2)
  df_task2$y_2=rep((df %>% dplyr::filter(year==2))$Y,each=2)
  #df_task2$y_3=(df %>% dplyr::filter(year==3))$Y
  df_task2$diff_1=rep((df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y,each=2)
  df_task2$size_1=rep((df %>% dplyr::filter(year==1))$n.patients,each=2)
  df_task2$size_2=rep((df %>% dplyr::filter(year==2))$n.patients,each=2)
  df_task2$year[df_task2$year==3]=0
  df_task2$year[df_task2$year==4]=1
  
  df_task1$X2=as.factor(df_task1$X2)
  df_task1$X4=as.factor(df_task1$X4)
  df_task1=BayesTree::makeind(df_task1)
  df_task1=as.data.frame(df_task1)
  
  df_task2$X2=as.factor(df_task2$X2)
  df_task2$X4=as.factor(df_task2$X4)
  df_task2=BayesTree::makeind(df_task2)
  df_task2=as.data.frame(df_task2)
  
  df_task1=df_task1 %>% select(-Y,-n.patients, -V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  
  X_task2_year3=df_task2 %>% select(-id.practice,-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  #-V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  num_patients_year3=(df_task2 %>% select(n.patients))$n.patients
  y_task2_year3=df_task2 %>% select(Y)
  y_task2_year3=y_task2_year3$Y
  #N=nrow(df_task2)
  N_0=length(df_task2$id.practice)
  
  #cv_folds_year3_train=createFolds(which(df_task2$year==3),k=5,list=TRUE,returnTrain=TRUE)
  #cv_folds_year4_train=createFolds(which(df_task2$year==4),k=5,list=TRUE,returnTrain=TRUE)
  #cv_folds_year3_test=createFolds(which(df_task2$year==0),k=5,list=TRUE,returnTrain=FALSE)
  #cv_folds_year4_test=createFolds(which(df_task2$year==1),k=5,list=TRUE,returnTrain=FALSE)
  cv_folds_train=createFolds(1:N_0,k=5,list=TRUE,returnTrain=TRUE)
  theta=vector(mode="numeric",length=K)
  N_test=vector(mode="numeric",length=K)
  N_test_3=vector(mode="numeric",length=K)
  N_test_4=vector(mode="numeric",length=K)
  a=vector(mode="numeric",length=K)
  b=vector(mode="numeric",length=K)
  psi_squared_sum_overall=vector(mode="list",length=K)
  for (j in 1:K){
    #train_index=c(which(df_task2$year==3)[cv_folds_year3_train[[j]]],which(df_task2$year==4)[cv_folds_year4_train[[j]]])
    train_index=cv_folds_train[[j]]
    test_index=setdiff(1:N_0,train_index)
    #df_task1_train=df_task1[train_index,]
    #df_task2_cv=df_task2[train_index,]
    #m_Z=df_task2$p_score
    id_train=df_task2[train_index,]$id.practice
    id_test=df_task2[test_index,]$id.practice
    
    X_task1_train=df_task1 %>% dplyr::filter(id.practice %in% id_train) %>% select(-id.practice,-Z)
    y_task1_train=df_task1 %>% dplyr::filter(id.practice %in% id_train) %>% select(Z)
    #X_task1_train=df_task1[train_index,] %>% select(-id.practice,-Z)
    y_task1_f=(df_task1[id_train,] %>% select(Z))$Z
    y_task1_train=y_task1_train$Z
    #X_inter=do.call(rbind,lapply(id_test,function(x) subset(df_task1,id.practice==x)))
    X_task1_test=df_task1[id_test,] %>% select(-id.practice,-Z)
    y_task1_test=df_task1[id_test,] %>% select(Z)
    #X_task1_test=df_task1[test_index,] %>% select(-id.practice,-Z)
    #y_task1_test=df_task1[test_index,] %>% select(Z)
    y_task1_test=y_task1_test$Z
    #train_len=length(train_index)
    test_len=length(test_index)
    
    bart_task1= BART::pbart(x.train=X_task1_train, y.train=y_task1_train, x.test=X_task1_test, ntree=100,ndpost=1000,nskip=250)
    f_x=bart_task1$yhat.test
    p_score_all=apply(f_x,c(1,2),plogis)
    p_score=apply(p_score_all,2,mean)
    
    X_0_year3=X_task2_year3[test_index,]
    X_0_year3$Z=rep(0,test_len)
    
    bart_1_year3=BART::gbart(x.train=X_task2_year3[train_index,], y.train=y_task2_year3[train_index],x.test=X_0_year3,type='wbart',ntree=100,ndpost=1000,nskip=500)
    Y_0_year3=bart_1_year3$yhat.test.mean
    
    #X_0=X_task2[test_index,]
    #X_0$Z=rep(0,test_len)
    #bart_1=BART::gbart(x.train=X_task2[train_index,], y.train=y_task2[train_index],x.test=X_0,type='wbart',ntree=100,ndpost=1000,nskip=500)
    #Y_0=bart_1$yhat.test.mean
    
  
    #forming score equation:
    N_test[j]=sum(num_patients_year3[test_index])
    N_train=sum(num_patients_year3[train_index])
    D_est=sum(y_task1_f*num_patients_year3[train_index])/N_train
    b[j]=sum((y_task2_year3[test_index]-Y_0_year3)*y_task1_test*num_patients_year3[test_index])/D_est - sum(p_score*(1-y_task1_test)*(y_task2_year3[test_index]-Y_0_year3)*num_patients_year3[test_index]/(D_est*(1-p_score)))
    a[j]=sum(y_task1_test*num_patients_year3[test_index])/D_est
    theta[j]=solve(a[j],b[j])
    b_el_1=(y_task2_year3[test_index]-Y_0_year3)*y_task1_test/D_est - p_score*(1-y_task1_test)*(y_task2_year3[test_index]-Y_0_year3)/(D_est*(1-p_score))
    a_el=y_task1_test/D_est
    psi_squared_sum_overall[[j]]=function(x) sum((b_el_1-a_el*x)^2*num_patients_year3[test_index])
    
  }  
  #num_tr_3=sum((df_task2 %>% dplyr::filter(Z==1 & year==3))$n.patients)
  #num_tr_4=sum((df_task2 %>% dplyr::filter(Z==1 & year==4))$n.patients)
  #theta_est=mean(theta)
  theta_est=sum(theta*N_test)/sum(N_test)
  SATT_overall=theta_est
  df_record$satt[count+1]=SATT_overall
  psi_overall=0
  for (h in 1:K){
    psi_overall=psi_overall+psi_squared_sum_overall[[h]](SATT_overall)
  }
  
  sigma_overall=sqrt(psi_overall)/sum(a)
  
  df_record$lower90[count+1]=SATT_overall-qnorm(1-alpha/2)*sigma_overall
  df_record$upper90[count+1]=SATT_overall+qnorm(1-alpha/2)*sigma_overall
  
  count=count+15
  write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/dml_time_record.csv",row.names = FALSE) 
}
