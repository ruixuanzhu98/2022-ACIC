library(BART)
library(BayesTree)
library(tidyverse)
library(caret)

count=0
K1=5
K2=3
K3=2
alpha=0.1

for (i in 1:3400){
  path=str_interp("/Users/piglet/Desktop/track2_20220404/merged/acic_practice_${i}.csv")
  print(i)
  df=read.csv(path)
  df_record=read.csv("/Users/piglet/Desktop/track2_20220404/dml_test_record.csv")
  
  df_task1=df %>% dplyr::filter(year==2) %>% select(-year,-post,-p_score)
  df_task2=df %>% dplyr::filter(year %in% c(3,4)) %>% select(-post,-p_score)
  
  df_task1$y_1=(df %>% dplyr::filter(year==1))$Y
  df_task1$y_2=(df %>% dplyr::filter(year==2))$Y
  df_task1$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_task1$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_task1$size_2=(df %>% dplyr::filter(year==2))$n.patients
  
  #df_task2$y_1=(df %>% dplyr::filter(year==1))$Y
  #df_task2$y_2=(df %>% dplyr::filter(year==2))$Y
  #df_task2$y_3=(df %>% dplyr::filter(year==3))$Y
  #df_task2$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  #df_task2$size_1=(df %>% dplyr::filter(year==1))$n.patients
  #df_task2$size_2=(df %>% dplyr::filter(year==2))$n.patients
  #df_task2$year[df_task2$year==3]=0
  #df_task2$year[df_task2$year==4]=1
  
  df_task1$X2=as.factor(df_task1$X2)
  df_task1$X4=as.factor(df_task1$X4)
  df_task1=BayesTree::makeind(df_task1)
  df_task1=as.data.frame(df_task1)
  
  df_task2$X2=as.factor(df_task2$X2)
  df_task2$X4=as.factor(df_task2$X4)
  df_task2=BayesTree::makeind(df_task2)
  df_task2=as.data.frame(df_task2)
  
  df_task1=df_task1 %>% select(-Y,-n.patients, -V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_task2_year3=df_task2 %>% dplyr::filter(year==3) %>% select(-id.practice,-year, -Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  #-V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_task2_year3$y_1=(df %>% dplyr::filter(year==1))$Y
  X_task2_year3$y_2=(df %>% dplyr::filter(year==2))$Y
  X_task2_year3$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  X_task2_year3$size_1=(df %>% dplyr::filter(year==1))$n.patients
  X_task2_year3$size_2=(df %>% dplyr::filter(year==2))$n.patients
  num_patients_year3=(df_task2 %>% dplyr::filter(year==3) %>% select(n.patients))$n.patients
  y_task2_year3=df_task2 %>% dplyr::filter(year==3) %>% select(Y)
  y_task2_year3=y_task2_year3$Y
  
  X_task2_year4=df_task2 %>% dplyr::filter(year==4) %>% select(-id.practice,-year,-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  #-V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_task2_year4$y_1=(df %>% dplyr::filter(year==1))$Y
  X_task2_year4$y_2=(df %>% dplyr::filter(year==2))$Y
  X_task2_year4$y_3=(df %>% dplyr::filter(year==3))$Y
  X_task2_year4$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  X_task2_year4$size_1=(df %>% dplyr::filter(year==1))$n.patients
  X_task2_year4$size_2=(df %>% dplyr::filter(year==2))$n.patients
  num_patients_year4=(df_task2 %>% dplyr::filter(year==4) %>% select(n.patients))$n.patients
  y_task2_year4=df_task2 %>% dplyr::filter(year==4) %>% select(Y)
  y_task2_year4=y_task2_year4$Y
 
  #X_task2_year3=X_task2_year3[X_task2_year3$X4.C==1,]
  #X_task2_year4=X_task2_year4[X_task2_year4$X4.C==1,]

  
  #X_task1_train=df_task1 %>% dplyr::filter(id.practice %in% id_train) %>% select(-id.practice,-Z)
  #y_task1_train=df_task1 %>% dplyr::filter(id.practice %in% id_train) %>% select(Z)
  X_task1_train=df_task1 %>% select(-id.practice,-Z)
  y_task1_train=df_task1 %>% select(Z)
  y_task1_train=y_task1_train$Z
  #X_inter=do.call(rbind,lapply(id_test,function(x) subset(df_task1,id.practice==x)))
  #X_task1_test=df_task1[id_test,] %>% select(-id.practice,-Z)
  #y_task1_test=df_task1[id_test,] %>% select(Z)
  
  bart_task1= BART::pbart(x.train=X_task1_train, y.train=y_task1_train, ntree=100,ndpost=1000,nskip=250)
  f_x=bart_task1$yhat.train
  p_score_all=apply(f_x,c(1,2),plogis)
  p_score=apply(p_score_all,2,mean)
  
  X_0_year3=X_task2_year3
  X_0_year3$Z=rep(0,nrow(X_0_year3))
  bart_1_year3=BART::gbart(x.train=X_task2_year3, y.train=y_task2_year3,x.test=X_0_year3,type='wbart',ntree=100,ndpost=1000,nskip=500)
  Y_0_year3=bart_1_year3$yhat.test.mean
  
  X_0_year4=X_task2_year4
  X_0_year4$Z=rep(0,nrow(X_0_year4))
  bart_1_year4=BART::gbart(x.train=X_task2_year4, y.train=y_task2_year4,x.test=X_0_year4,type='wbart',ntree=100,ndpost=1000,nskip=500)
  Y_0_year4=bart_1_year4$yhat.test.mean
  
  
  #N=nrow(df_task2)
  N_overall=df_task1$id.practice
  N_X1_0=(df_task1 %>% dplyr::filter(X1==0))$id.practice
  N_X1_1=(df_task1 %>% dplyr::filter(X1==1))$id.practice
  N_X2_A=(df_task1 %>% dplyr::filter(X2.A==1))$id.practice
  N_X2_B=(df_task1 %>% dplyr::filter(X2.B==1))$id.practice
  N_X2_C=(df_task1 %>% dplyr::filter(X2.C==1))$id.practice
  N_X3_0=(df_task1 %>% dplyr::filter(X3==0))$id.practice
  N_X3_1=(df_task1 %>% dplyr::filter(X3==1))$id.practice
  N_X4_A=(df_task1 %>% dplyr::filter(X4.A==1))$id.practice
  N_X4_B=(df_task1 %>% dplyr::filter(X4.B==1))$id.practice
  N_X4_C=(df_task1 %>% dplyr::filter(X4.C==1))$id.practice
  N_X5_0=(df_task1 %>% dplyr::filter(X5==0))$id.practice
  N_X5_1=(df_task1 %>% dplyr::filter(X5==1))$id.practice
  
  #cv_folds_year3_train=createFolds(which(df_task2$year==3),k=5,list=TRUE,returnTrain=TRUE)
  #cv_folds_year4_train=createFolds(which(df_task2$year==4),k=5,list=TRUE,returnTrain=TRUE)
  #cv_folds_year3_test=createFolds(which(df_task2$year==0),k=5,list=TRUE,returnTrain=FALSE)
  #cv_folds_year4_test=createFolds(which(df_task2$year==1),k=5,list=TRUE,returnTrain=FALSE)
  cv_folds_train_overall=createFolds(N_overall,k=5,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X1_0=createFolds(N_X1_0,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X1_1=createFolds(N_X1_1,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X2_A=createFolds(N_X2_A,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X2_B=createFolds(N_X2_B,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X2_C=createFolds(N_X2_C,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X3_0=createFolds(N_X3_0,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X3_1=createFolds(N_X3_1,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X4_A=createFolds(N_X4_A,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X4_B=createFolds(N_X4_B,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X4_C=createFolds(N_X4_C,k=2,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X5_0=createFolds(N_X5_0,k=3,list=TRUE,returnTrain=TRUE)
  cv_folds_train_X5_1=createFolds(N_X5_1,k=3,list=TRUE,returnTrain=TRUE)
  
  theta_X1_0=vector(mode="numeric",length=K2)
  theta_X1_1=vector(mode="numeric",length=K2)
  theta_X2_A=vector(mode="numeric",length=K2)
  theta_X2_B=vector(mode="numeric",length=K2)
  theta_X2_C=vector(mode="numeric",length=K2)
  theta_X3_0=vector(mode="numeric",length=K2)
  theta_X3_1=vector(mode="numeric",length=K2)
  theta_X4_A=vector(mode="numeric",length=K2)
  theta_X4_B=vector(mode="numeric",length=K2)
  theta_X4_C=vector(mode="numeric",length=K3)
  theta_X5_0=vector(mode="numeric",length=K2)
  theta_X5_1=vector(mode="numeric",length=K2)
  
  N_test_overall=vector(mode="numeric",length=K1)
  N_test_X1_0=vector(mode="numeric",length=K2)
  N_test_X1_1=vector(mode="numeric",length=K2)
  N_test_X2_A=vector(mode="numeric",length=K2)
  N_test_X2_B=vector(mode="numeric",length=K2)
  N_test_X2_C=vector(mode="numeric",length=K2)
  N_test_X3_0=vector(mode="numeric",length=K2)
  N_test_X3_1=vector(mode="numeric",length=K2)
  N_test_X4_A=vector(mode="numeric",length=K2)
  N_test_X4_B=vector(mode="numeric",length=K2)
  N_test_X4_C=vector(mode="numeric",length=K3)
  N_test_X5_0=vector(mode="numeric",length=K2)
  N_test_X5_1=vector(mode="numeric",length=K2)
  
  psi_squared_sum_overall=vector(mode="list",length=K1)
  psi_squared_sum_X1_0=vector(mode="list",length=K2)
  psi_squared_sum_X1_1=vector(mode="list",length=K2)
  psi_squared_sum_X2_A=vector(mode="list",length=K2)
  psi_squared_sum_X2_B=vector(mode="list",length=K2)
  psi_squared_sum_X2_C=vector(mode="list",length=K2)
  psi_squared_sum_X3_0=vector(mode="list",length=K2)
  psi_squared_sum_X3_1=vector(mode="list",length=K2)
  psi_squared_sum_X4_A=vector(mode="list",length=K2)
  psi_squared_sum_X4_B=vector(mode="list",length=K2)
  psi_squared_sum_X4_C=vector(mode="list",length=K3)
  psi_squared_sum_X5_0=vector(mode="list",length=K2)
  psi_squared_sum_X5_1=vector(mode="list",length=K2)
  
  a_overall=vector(mode="numeric",length=K1)
  a_X1_0=vector(mode="numeric",length=K2)
  a_X1_1=vector(mode="numeric",length=K2)
  a_X2_A=vector(mode="numeric",length=K2)
  a_X2_B=vector(mode="numeric",length=K2)
  a_X2_C=vector(mode="numeric",length=K2)
  a_X3_0=vector(mode="numeric",length=K2)
  a_X3_1=vector(mode="numeric",length=K2)
  a_X4_A=vector(mode="numeric",length=K2)
  a_X4_B=vector(mode="numeric",length=K2)
  a_X4_C=vector(mode="numeric",length=K3)
  a_X5_0=vector(mode="numeric",length=K2)
  a_X5_1=vector(mode="numeric",length=K2)
  for (j in 1:K2){
    #train_index=c(which(df_task2$year==3)[cv_folds_year3_train[[j]]],which(df_task2$year==4)[cv_folds_year4_train[[j]]])
    
    train_index_X1_0=N_X1_0[cv_folds_train_X1_0[[j]]]
    test_index_X1_0=N_X1_0[-cv_folds_train_X1_0[[j]]]
    train_index_X1_1=N_X1_1[cv_folds_train_X1_1[[j]]]
    test_index_X1_1=N_X1_1[-cv_folds_train_X1_1[[j]]]
    train_index_X2_A=N_X2_A[cv_folds_train_X2_A[[j]]]
    test_index_X2_A=N_X2_A[-cv_folds_train_X2_A[[j]]]
    train_index_X2_B=N_X2_B[cv_folds_train_X2_B[[j]]]
    test_index_X2_B=N_X2_B[-cv_folds_train_X2_B[[j]]]
    train_index_X2_C=N_X2_C[cv_folds_train_X2_C[[j]]]
    test_index_X2_C=N_X2_C[-cv_folds_train_X2_C[[j]]]
    train_index_X3_0=N_X3_0[cv_folds_train_X3_0[[j]]]
    test_index_X3_0=N_X3_0[-cv_folds_train_X3_0[[j]]]
    train_index_X3_1=N_X3_1[cv_folds_train_X3_1[[j]]]
    test_index_X3_1=N_X3_1[-cv_folds_train_X3_1[[j]]]
    train_index_X4_A=N_X4_A[cv_folds_train_X4_A[[j]]]
    test_index_X4_A=N_X4_A[-cv_folds_train_X4_A[[j]]]
    train_index_X4_B=N_X4_B[cv_folds_train_X4_B[[j]]]
    test_index_X4_B=N_X4_B[-cv_folds_train_X4_B[[j]]]
    
    train_index_X5_0=N_X5_0[cv_folds_train_X5_0[[j]]]
    test_index_X5_0=N_X5_0[-cv_folds_train_X5_0[[j]]]
    train_index_X5_1=N_X5_1[cv_folds_train_X5_1[[j]]]
    test_index_X5_1=N_X5_1[-cv_folds_train_X5_1[[j]]]
    
    #forming score equation:
    #D_est could be zero if K=5 for subgroup
    
    #X1_0:
    N_test_X1_0[j]=sum(num_patients_year3[test_index_X1_0]+num_patients_year4[test_index_X1_0])
    N_train=sum(num_patients_year3[train_index_X1_0]+num_patients_year4[train_index_X1_0])
    D_est=sum(y_task1_train[train_index_X1_0]*num_patients_year3[train_index_X1_0]+y_task1_train[train_index_X1_0]*num_patients_year4[train_index_X1_0])/N_train
    b_1=sum((y_task2_year3[test_index_X1_0]-Y_0_year3[test_index_X1_0])*y_task1_train[test_index_X1_0]*num_patients_year3[test_index_X1_0])/D_est - sum(p_score[test_index_X1_0]*(1-y_task1_train[test_index_X1_0])*(y_task2_year3[test_index_X1_0]-Y_0_year3[test_index_X1_0])*num_patients_year3[test_index_X1_0]/(D_est*(1-p_score[test_index_X1_0])))
    b_2=sum((y_task2_year4[test_index_X1_0]-Y_0_year4[test_index_X1_0])*y_task1_train[test_index_X1_0]*num_patients_year4[test_index_X1_0])/D_est - sum(p_score[test_index_X1_0]*(1-y_task1_train[test_index_X1_0])*(y_task2_year4[test_index_X1_0]-Y_0_year4[test_index_X1_0])*num_patients_year4[test_index_X1_0]/(D_est*(1-p_score[test_index_X1_0])))
    b=b_1+b_2
    a_X1_0[j]=sum(y_task1_train[test_index_X1_0]*num_patients_year3[test_index_X1_0]+y_task1_train[test_index_X1_0]*num_patients_year4[test_index_X1_0])/D_est
    theta_X1_0[j]=solve(a_X1_0[j],b)
    b_el_1_4=(y_task2_year3[test_index_X1_0]-Y_0_year3[test_index_X1_0])*y_task1_train[test_index_X1_0]/D_est - p_score[test_index_X1_0]*(1-y_task1_train[test_index_X1_0])*(y_task2_year3[test_index_X1_0]-Y_0_year3[test_index_X1_0])/(D_est*(1-p_score[test_index_X1_0]))
    b_el_2_4=(y_task2_year4[test_index_X1_0]-Y_0_year4[test_index_X1_0])*y_task1_train[test_index_X1_0]/D_est - p_score[test_index_X1_0]*(1-y_task1_train[test_index_X1_0])*(y_task2_year4[test_index_X1_0]-Y_0_year4[test_index_X1_0])/(D_est*(1-p_score[test_index_X1_0]))
    a_el_4=y_task1_train[test_index_X1_0]/D_est
    psi_squared_sum_X1_0[[j]]=function(x) sum((b_el_1_4-a_el_4*x)^2*num_patients_year3[test_index_X1_0])+sum((b_el_2_4-a_el_4*x)^2*num_patients_year4[test_index_X1_0])
    
    #X1_1:
    N_test_X1_1[j]=sum(num_patients_year3[test_index_X1_1]+num_patients_year4[test_index_X1_1])
    N_train=sum(num_patients_year3[train_index_X1_1]+num_patients_year4[train_index_X1_1])
    D_est=sum(y_task1_train[train_index_X1_1]*num_patients_year3[train_index_X1_1]+y_task1_train[train_index_X1_1]*num_patients_year4[train_index_X1_1])/N_train
    b_1=sum((y_task2_year3[test_index_X1_1]-Y_0_year3[test_index_X1_1])*y_task1_train[test_index_X1_1]*num_patients_year3[test_index_X1_1])/D_est - sum(p_score[test_index_X1_1]*(1-y_task1_train[test_index_X1_1])*(y_task2_year3[test_index_X1_1]-Y_0_year3[test_index_X1_1])*num_patients_year3[test_index_X1_1]/(D_est*(1-p_score[test_index_X1_1])))
    b_2=sum((y_task2_year4[test_index_X1_1]-Y_0_year4[test_index_X1_1])*y_task1_train[test_index_X1_1]*num_patients_year4[test_index_X1_1])/D_est - sum(p_score[test_index_X1_1]*(1-y_task1_train[test_index_X1_1])*(y_task2_year4[test_index_X1_1]-Y_0_year4[test_index_X1_1])*num_patients_year4[test_index_X1_1]/(D_est*(1-p_score[test_index_X1_1])))
    b=b_1+b_2
    a_X1_1[j]=sum(y_task1_train[test_index_X1_1]*num_patients_year3[test_index_X1_1]+y_task1_train[test_index_X1_1]*num_patients_year4[test_index_X1_1])/D_est
    theta_X1_1[j]=solve(a_X1_1[j],b)
    b_el_1_5=(y_task2_year3[test_index_X1_1]-Y_0_year3[test_index_X1_1])*y_task1_train[test_index_X1_1]/D_est - p_score[test_index_X1_1]*(1-y_task1_train[test_index_X1_1])*(y_task2_year3[test_index_X1_1]-Y_0_year3[test_index_X1_1])/(D_est*(1-p_score[test_index_X1_1]))
    b_el_2_5=(y_task2_year4[test_index_X1_1]-Y_0_year4[test_index_X1_1])*y_task1_train[test_index_X1_1]/D_est - p_score[test_index_X1_1]*(1-y_task1_train[test_index_X1_1])*(y_task2_year4[test_index_X1_1]-Y_0_year4[test_index_X1_1])/(D_est*(1-p_score[test_index_X1_1]))
    a_el_5=y_task1_train[test_index_X1_1]/D_est
    psi_squared_sum_X1_1[[j]]=function(x) sum((b_el_1_5-a_el_5*x)^2*num_patients_year3[test_index_X1_1])+sum((b_el_2_5-a_el_5*x)^2*num_patients_year4[test_index_X1_1])
    
    #X2_A:
    N_test_X2_A[j]=sum(num_patients_year3[test_index_X2_A]+num_patients_year4[test_index_X2_A])
    N_train=sum(num_patients_year3[train_index_X2_A]+num_patients_year4[train_index_X2_A])
    D_est=sum(y_task1_train[train_index_X2_A]*num_patients_year3[train_index_X2_A]+y_task1_train[train_index_X2_A]*num_patients_year4[train_index_X2_A])/N_train
    b_1=sum((y_task2_year3[test_index_X2_A]-Y_0_year3[test_index_X2_A])*y_task1_train[test_index_X2_A]*num_patients_year3[test_index_X2_A])/D_est - sum(p_score[test_index_X2_A]*(1-y_task1_train[test_index_X2_A])*(y_task2_year3[test_index_X2_A]-Y_0_year3[test_index_X2_A])*num_patients_year3[test_index_X2_A]/(D_est*(1-p_score[test_index_X2_A])))
    b_2=sum((y_task2_year4[test_index_X2_A]-Y_0_year4[test_index_X2_A])*y_task1_train[test_index_X2_A]*num_patients_year4[test_index_X2_A])/D_est - sum(p_score[test_index_X2_A]*(1-y_task1_train[test_index_X2_A])*(y_task2_year4[test_index_X2_A]-Y_0_year4[test_index_X2_A])*num_patients_year4[test_index_X2_A]/(D_est*(1-p_score[test_index_X2_A])))
    b=b_1+b_2
    a_X2_A[j]=sum(y_task1_train[test_index_X2_A]*num_patients_year3[test_index_X2_A]+y_task1_train[test_index_X2_A]*num_patients_year4[test_index_X2_A])/D_est
    theta_X2_A[j]=solve(a_X2_A[j],b)
    b_el_1_6=(y_task2_year3[test_index_X2_A]-Y_0_year3[test_index_X2_A])*y_task1_train[test_index_X2_A]/D_est - p_score[test_index_X2_A]*(1-y_task1_train[test_index_X2_A])*(y_task2_year3[test_index_X2_A]-Y_0_year3[test_index_X2_A])/(D_est*(1-p_score[test_index_X2_A]))
    b_el_2_6=(y_task2_year4[test_index_X2_A]-Y_0_year4[test_index_X2_A])*y_task1_train[test_index_X2_A]/D_est - p_score[test_index_X2_A]*(1-y_task1_train[test_index_X2_A])*(y_task2_year4[test_index_X2_A]-Y_0_year4[test_index_X2_A])/(D_est*(1-p_score[test_index_X2_A]))
    a_el_6=y_task1_train[test_index_X2_A]/D_est
    psi_squared_sum_X2_A[[j]]=function(x) sum((b_el_1_6-a_el_6*x)^2*num_patients_year3[test_index_X2_A])+sum((b_el_2_6-a_el_6*x)^2*num_patients_year4[test_index_X2_A])
    
    #X2_B:
    N_test_X2_B[j]=sum(num_patients_year3[test_index_X2_B]+num_patients_year4[test_index_X2_B])
    N_train=sum(num_patients_year3[train_index_X2_B]+num_patients_year4[train_index_X2_B])
    D_est=sum(y_task1_train[train_index_X2_B]*num_patients_year3[train_index_X2_B]+y_task1_train[train_index_X2_B]*num_patients_year4[train_index_X2_B])/N_train
    b_1=sum((y_task2_year3[test_index_X2_B]-Y_0_year3[test_index_X2_B])*y_task1_train[test_index_X2_B]*num_patients_year3[test_index_X2_B])/D_est - sum(p_score[test_index_X2_B]*(1-y_task1_train[test_index_X2_B])*(y_task2_year3[test_index_X2_B]-Y_0_year3[test_index_X2_B])*num_patients_year3[test_index_X2_B]/(D_est*(1-p_score[test_index_X2_B])))
    b_2=sum((y_task2_year4[test_index_X2_B]-Y_0_year4[test_index_X2_B])*y_task1_train[test_index_X2_B]*num_patients_year4[test_index_X2_B])/D_est - sum(p_score[test_index_X2_B]*(1-y_task1_train[test_index_X2_B])*(y_task2_year4[test_index_X2_B]-Y_0_year4[test_index_X2_B])*num_patients_year4[test_index_X2_B]/(D_est*(1-p_score[test_index_X2_B])))
    b=b_1+b_2
    a_X2_B[j]=sum(y_task1_train[test_index_X2_B]*num_patients_year3[test_index_X2_B]+y_task1_train[test_index_X2_B]*num_patients_year4[test_index_X2_B])/D_est
    theta_X2_B[j]=solve(a_X2_B[j],b)
    b_el_1_7=(y_task2_year3[test_index_X2_B]-Y_0_year3[test_index_X2_B])*y_task1_train[test_index_X2_B]/D_est - p_score[test_index_X2_B]*(1-y_task1_train[test_index_X2_B])*(y_task2_year3[test_index_X2_B]-Y_0_year3[test_index_X2_B])/(D_est*(1-p_score[test_index_X2_B]))
    b_el_2_7=(y_task2_year4[test_index_X2_B]-Y_0_year4[test_index_X2_B])*y_task1_train[test_index_X2_B]/D_est - p_score[test_index_X2_B]*(1-y_task1_train[test_index_X2_B])*(y_task2_year4[test_index_X2_B]-Y_0_year4[test_index_X2_B])/(D_est*(1-p_score[test_index_X2_B]))
    a_el_7=y_task1_train[test_index_X2_B]/D_est
    psi_squared_sum_X2_B[[j]]=function(x) sum((b_el_1_7-a_el_7*x)^2*num_patients_year3[test_index_X2_B])+sum((b_el_2_7-a_el_7*x)^2*num_patients_year4[test_index_X2_B])
    
    #X2_C:
    N_test_X2_C[j]=sum(num_patients_year3[test_index_X2_C]+num_patients_year4[test_index_X2_C])
    N_train=sum(num_patients_year3[train_index_X2_C]+num_patients_year4[train_index_X2_C])
    D_est=sum(y_task1_train[train_index_X2_C]*num_patients_year3[train_index_X2_C]+y_task1_train[train_index_X2_C]*num_patients_year4[train_index_X2_C])/N_train
    b_1=sum((y_task2_year3[test_index_X2_C]-Y_0_year3[test_index_X2_C])*y_task1_train[test_index_X2_C]*num_patients_year3[test_index_X2_C])/D_est - sum(p_score[test_index_X2_C]*(1-y_task1_train[test_index_X2_C])*(y_task2_year3[test_index_X2_C]-Y_0_year3[test_index_X2_C])*num_patients_year3[test_index_X2_C]/(D_est*(1-p_score[test_index_X2_C])))
    b_2=sum((y_task2_year4[test_index_X2_C]-Y_0_year4[test_index_X2_C])*y_task1_train[test_index_X2_C]*num_patients_year4[test_index_X2_C])/D_est - sum(p_score[test_index_X2_C]*(1-y_task1_train[test_index_X2_C])*(y_task2_year4[test_index_X2_C]-Y_0_year4[test_index_X2_C])*num_patients_year4[test_index_X2_C]/(D_est*(1-p_score[test_index_X2_C])))
    b=b_1+b_2
    a_X2_C[j]=sum(y_task1_train[test_index_X2_C]*num_patients_year3[test_index_X2_C]+y_task1_train[test_index_X2_C]*num_patients_year4[test_index_X2_C])/D_est
    theta_X2_C[j]=solve(a_X2_C[j],b)
    b_el_1_8=(y_task2_year3[test_index_X2_C]-Y_0_year3[test_index_X2_C])*y_task1_train[test_index_X2_C]/D_est - p_score[test_index_X2_C]*(1-y_task1_train[test_index_X2_C])*(y_task2_year3[test_index_X2_C]-Y_0_year3[test_index_X2_C])/(D_est*(1-p_score[test_index_X2_C]))
    b_el_2_8=(y_task2_year4[test_index_X2_C]-Y_0_year4[test_index_X2_C])*y_task1_train[test_index_X2_C]/D_est - p_score[test_index_X2_C]*(1-y_task1_train[test_index_X2_C])*(y_task2_year4[test_index_X2_C]-Y_0_year4[test_index_X2_C])/(D_est*(1-p_score[test_index_X2_C]))
    a_el_8=y_task1_train[test_index_X2_C]/D_est
    psi_squared_sum_X2_C[[j]]=function(x) sum((b_el_1_8-a_el_8*x)^2*num_patients_year3[test_index_X2_C])+sum((b_el_2_8-a_el_8*x)^2*num_patients_year4[test_index_X2_C])
    
    #X3_0:
    N_test_X3_0[j]=sum(num_patients_year3[test_index_X3_0]+num_patients_year4[test_index_X3_0])
    N_train=sum(num_patients_year3[train_index_X3_0]+num_patients_year4[train_index_X3_0])
    D_est=sum(y_task1_train[train_index_X3_0]*num_patients_year3[train_index_X3_0]+y_task1_train[train_index_X3_0]*num_patients_year4[train_index_X3_0])/N_train
    b_1=sum((y_task2_year3[test_index_X3_0]-Y_0_year3[test_index_X3_0])*y_task1_train[test_index_X3_0]*num_patients_year3[test_index_X3_0])/D_est - sum(p_score[test_index_X3_0]*(1-y_task1_train[test_index_X3_0])*(y_task2_year3[test_index_X3_0]-Y_0_year3[test_index_X3_0])*num_patients_year3[test_index_X3_0]/(D_est*(1-p_score[test_index_X3_0])))
    b_2=sum((y_task2_year4[test_index_X3_0]-Y_0_year4[test_index_X3_0])*y_task1_train[test_index_X3_0]*num_patients_year4[test_index_X3_0])/D_est - sum(p_score[test_index_X3_0]*(1-y_task1_train[test_index_X3_0])*(y_task2_year4[test_index_X3_0]-Y_0_year4[test_index_X3_0])*num_patients_year4[test_index_X3_0]/(D_est*(1-p_score[test_index_X3_0])))
    b=b_1+b_2
    a_X3_0[j]=sum(y_task1_train[test_index_X3_0]*num_patients_year3[test_index_X3_0]+y_task1_train[test_index_X3_0]*num_patients_year4[test_index_X3_0])/D_est
    theta_X3_0[j]=solve(a_X3_0[j],b)
    b_el_1_9=(y_task2_year3[test_index_X3_0]-Y_0_year3[test_index_X3_0])*y_task1_train[test_index_X3_0]/D_est - p_score[test_index_X3_0]*(1-y_task1_train[test_index_X3_0])*(y_task2_year3[test_index_X3_0]-Y_0_year3[test_index_X3_0])/(D_est*(1-p_score[test_index_X3_0]))
    b_el_2_9=(y_task2_year4[test_index_X3_0]-Y_0_year4[test_index_X3_0])*y_task1_train[test_index_X3_0]/D_est - p_score[test_index_X3_0]*(1-y_task1_train[test_index_X3_0])*(y_task2_year4[test_index_X3_0]-Y_0_year4[test_index_X3_0])/(D_est*(1-p_score[test_index_X3_0]))
    a_el_9=y_task1_train[test_index_X3_0]/D_est
    psi_squared_sum_X3_0[[j]]=function(x) sum((b_el_1_9-a_el_9*x)^2*num_patients_year3[test_index_X3_0])+sum((b_el_2_9-a_el_9*x)^2*num_patients_year4[test_index_X3_0])
    
    #X3_1:
    N_test_X3_1[j]=sum(num_patients_year3[test_index_X3_1]+num_patients_year4[test_index_X3_1])
    N_train=sum(num_patients_year3[train_index_X3_1]+num_patients_year4[train_index_X3_1])
    D_est=sum(y_task1_train[train_index_X3_1]*num_patients_year3[train_index_X3_1]+y_task1_train[train_index_X3_1]*num_patients_year4[train_index_X3_1])/N_train
    b_1=sum((y_task2_year3[test_index_X3_1]-Y_0_year3[test_index_X3_1])*y_task1_train[test_index_X3_1]*num_patients_year3[test_index_X3_1])/D_est - sum(p_score[test_index_X3_1]*(1-y_task1_train[test_index_X3_1])*(y_task2_year3[test_index_X3_1]-Y_0_year3[test_index_X3_1])*num_patients_year3[test_index_X3_1]/(D_est*(1-p_score[test_index_X3_1])))
    b_2=sum((y_task2_year4[test_index_X3_1]-Y_0_year4[test_index_X3_1])*y_task1_train[test_index_X3_1]*num_patients_year4[test_index_X3_1])/D_est - sum(p_score[test_index_X3_1]*(1-y_task1_train[test_index_X3_1])*(y_task2_year4[test_index_X3_1]-Y_0_year4[test_index_X3_1])*num_patients_year4[test_index_X3_1]/(D_est*(1-p_score[test_index_X3_1])))
    b=b_1+b_2
    a_X3_1[j]=sum(y_task1_train[test_index_X3_1]*num_patients_year3[test_index_X3_1]+y_task1_train[test_index_X3_1]*num_patients_year4[test_index_X3_1])/D_est
    theta_X3_1[j]=solve(a_X3_1[j],b)
    b_el_1_10=(y_task2_year3[test_index_X3_1]-Y_0_year3[test_index_X3_1])*y_task1_train[test_index_X3_1]/D_est - p_score[test_index_X3_1]*(1-y_task1_train[test_index_X3_1])*(y_task2_year3[test_index_X3_1]-Y_0_year3[test_index_X3_1])/(D_est*(1-p_score[test_index_X3_1]))
    b_el_2_10=(y_task2_year4[test_index_X3_1]-Y_0_year4[test_index_X3_1])*y_task1_train[test_index_X3_1]/D_est - p_score[test_index_X3_1]*(1-y_task1_train[test_index_X3_1])*(y_task2_year4[test_index_X3_1]-Y_0_year4[test_index_X3_1])/(D_est*(1-p_score[test_index_X3_1]))
    a_el_10=y_task1_train[test_index_X3_1]/D_est
    psi_squared_sum_X3_1[[j]]=function(x) sum((b_el_1_10-a_el_10*x)^2*num_patients_year3[test_index_X3_1])+sum((b_el_2_10-a_el_10*x)^2*num_patients_year4[test_index_X3_1])
    
    #X4_A:
    N_test_X4_A[j]=sum(num_patients_year3[test_index_X4_A]+num_patients_year4[test_index_X4_A])
    N_train=sum(num_patients_year3[train_index_X4_A]+num_patients_year4[train_index_X4_A])
    D_est=sum(y_task1_train[train_index_X4_A]*num_patients_year3[train_index_X4_A]+y_task1_train[train_index_X4_A]*num_patients_year4[train_index_X4_A])/N_train
    b_1=sum((y_task2_year3[test_index_X4_A]-Y_0_year3[test_index_X4_A])*y_task1_train[test_index_X4_A]*num_patients_year3[test_index_X4_A])/D_est - sum(p_score[test_index_X4_A]*(1-y_task1_train[test_index_X4_A])*(y_task2_year3[test_index_X4_A]-Y_0_year3[test_index_X4_A])*num_patients_year3[test_index_X4_A]/(D_est*(1-p_score[test_index_X4_A])))
    b_2=sum((y_task2_year4[test_index_X4_A]-Y_0_year4[test_index_X4_A])*y_task1_train[test_index_X4_A]*num_patients_year4[test_index_X4_A])/D_est - sum(p_score[test_index_X4_A]*(1-y_task1_train[test_index_X4_A])*(y_task2_year4[test_index_X4_A]-Y_0_year4[test_index_X4_A])*num_patients_year4[test_index_X4_A]/(D_est*(1-p_score[test_index_X4_A])))
    b=b_1+b_2
    a_X4_A[j]=sum(y_task1_train[test_index_X4_A]*num_patients_year3[test_index_X4_A]+y_task1_train[test_index_X4_A]*num_patients_year4[test_index_X4_A])/D_est
    theta_X4_A[j]=solve(a_X4_A[j],b)
    b_el_1_11=(y_task2_year3[test_index_X4_A]-Y_0_year3[test_index_X4_A])*y_task1_train[test_index_X4_A]/D_est - p_score[test_index_X4_A]*(1-y_task1_train[test_index_X4_A])*(y_task2_year3[test_index_X4_A]-Y_0_year3[test_index_X4_A])/(D_est*(1-p_score[test_index_X4_A]))
    b_el_2_11=(y_task2_year4[test_index_X4_A]-Y_0_year4[test_index_X4_A])*y_task1_train[test_index_X4_A]/D_est - p_score[test_index_X4_A]*(1-y_task1_train[test_index_X4_A])*(y_task2_year4[test_index_X4_A]-Y_0_year4[test_index_X4_A])/(D_est*(1-p_score[test_index_X4_A]))
    a_el_11=y_task1_train[test_index_X4_A]/D_est
    psi_squared_sum_X4_A[[j]]=function(x) sum((b_el_1_11-a_el_11*x)^2*num_patients_year3[test_index_X4_A])+sum((b_el_2_11-a_el_11*x)^2*num_patients_year4[test_index_X4_A])
    
    #X4_B:
    N_test_X4_B[j]=sum(num_patients_year3[test_index_X4_B]+num_patients_year4[test_index_X4_B])
    N_train=sum(num_patients_year3[train_index_X4_B]+num_patients_year4[train_index_X4_B])
    D_est=sum(y_task1_train[train_index_X4_B]*num_patients_year3[train_index_X4_B]+y_task1_train[train_index_X4_B]*num_patients_year4[train_index_X4_B])/N_train
    b_1=sum((y_task2_year3[test_index_X4_B]-Y_0_year3[test_index_X4_B])*y_task1_train[test_index_X4_B]*num_patients_year3[test_index_X4_B])/D_est - sum(p_score[test_index_X4_B]*(1-y_task1_train[test_index_X4_B])*(y_task2_year3[test_index_X4_B]-Y_0_year3[test_index_X4_B])*num_patients_year3[test_index_X4_B]/(D_est*(1-p_score[test_index_X4_B])))
    b_2=sum((y_task2_year4[test_index_X4_B]-Y_0_year4[test_index_X4_B])*y_task1_train[test_index_X4_B]*num_patients_year4[test_index_X4_B])/D_est - sum(p_score[test_index_X4_B]*(1-y_task1_train[test_index_X4_B])*(y_task2_year4[test_index_X4_B]-Y_0_year4[test_index_X4_B])*num_patients_year4[test_index_X4_B]/(D_est*(1-p_score[test_index_X4_B])))
    b=b_1+b_2
    a_X4_B[j]=sum(y_task1_train[test_index_X4_B]*num_patients_year3[test_index_X4_B]+y_task1_train[test_index_X4_B]*num_patients_year4[test_index_X4_B])/D_est
    theta_X4_B[j]=solve(a_X4_B[j],b)
    b_el_1_12=(y_task2_year3[test_index_X4_B]-Y_0_year3[test_index_X4_B])*y_task1_train[test_index_X4_B]/D_est - p_score[test_index_X4_B]*(1-y_task1_train[test_index_X4_B])*(y_task2_year3[test_index_X4_B]-Y_0_year3[test_index_X4_B])/(D_est*(1-p_score[test_index_X4_B]))
    b_el_2_12=(y_task2_year4[test_index_X4_B]-Y_0_year4[test_index_X4_B])*y_task1_train[test_index_X4_B]/D_est - p_score[test_index_X4_B]*(1-y_task1_train[test_index_X4_B])*(y_task2_year4[test_index_X4_B]-Y_0_year4[test_index_X4_B])/(D_est*(1-p_score[test_index_X4_B]))
    a_el_12=y_task1_train[test_index_X4_B]/D_est
    psi_squared_sum_X4_B[[j]]=function(x) sum((b_el_1_12-a_el_12*x)^2*num_patients_year3[test_index_X4_B])+sum((b_el_2_12-a_el_12*x)^2*num_patients_year4[test_index_X4_B])
    
    
    
    #X5_0:
    N_test_X5_0[j]=sum(num_patients_year3[test_index_X5_0]+num_patients_year4[test_index_X5_0])
    N_train=sum(num_patients_year3[train_index_X5_0]+num_patients_year4[train_index_X5_0])
    D_est=sum(y_task1_train[train_index_X5_0]*num_patients_year3[train_index_X5_0]+y_task1_train[train_index_X5_0]*num_patients_year4[train_index_X5_0])/N_train
    b_1=sum((y_task2_year3[test_index_X5_0]-Y_0_year3[test_index_X5_0])*y_task1_train[test_index_X5_0]*num_patients_year3[test_index_X5_0])/D_est - sum(p_score[test_index_X5_0]*(1-y_task1_train[test_index_X5_0])*(y_task2_year3[test_index_X5_0]-Y_0_year3[test_index_X5_0])*num_patients_year3[test_index_X5_0]/(D_est*(1-p_score[test_index_X5_0])))
    b_2=sum((y_task2_year4[test_index_X5_0]-Y_0_year4[test_index_X5_0])*y_task1_train[test_index_X5_0]*num_patients_year4[test_index_X5_0])/D_est - sum(p_score[test_index_X5_0]*(1-y_task1_train[test_index_X5_0])*(y_task2_year4[test_index_X5_0]-Y_0_year4[test_index_X5_0])*num_patients_year4[test_index_X5_0]/(D_est*(1-p_score[test_index_X5_0])))
    b=b_1+b_2
    a_X5_0[j]=sum(y_task1_train[test_index_X5_0]*num_patients_year3[test_index_X5_0]+y_task1_train[test_index_X5_0]*num_patients_year4[test_index_X5_0])/D_est
    theta_X5_0[j]=solve(a_X5_0[j],b)
    b_el_1_14=(y_task2_year3[test_index_X5_0]-Y_0_year3[test_index_X5_0])*y_task1_train[test_index_X5_0]/D_est - p_score[test_index_X5_0]*(1-y_task1_train[test_index_X5_0])*(y_task2_year3[test_index_X5_0]-Y_0_year3[test_index_X5_0])/(D_est*(1-p_score[test_index_X5_0]))
    b_el_2_14=(y_task2_year4[test_index_X5_0]-Y_0_year4[test_index_X5_0])*y_task1_train[test_index_X5_0]/D_est - p_score[test_index_X5_0]*(1-y_task1_train[test_index_X5_0])*(y_task2_year4[test_index_X5_0]-Y_0_year4[test_index_X5_0])/(D_est*(1-p_score[test_index_X5_0]))
    a_el_14=y_task1_train[test_index_X5_0]/D_est
    psi_squared_sum_X5_0[[j]]=function(x) sum((b_el_1_14-a_el_14*x)^2*num_patients_year3[test_index_X5_0])+sum((b_el_2_14-a_el_14*x)^2*num_patients_year4[test_index_X5_0])
    
    #X5_1:
    N_test_X5_1[j]=sum(num_patients_year3[test_index_X5_1]+num_patients_year4[test_index_X5_1])
    N_train=sum(num_patients_year3[train_index_X5_1]+num_patients_year4[train_index_X5_1])
    D_est=sum(y_task1_train[train_index_X5_1]*num_patients_year3[train_index_X5_1]+y_task1_train[train_index_X5_1]*num_patients_year4[train_index_X5_1])/N_train
    b_1=sum((y_task2_year3[test_index_X5_1]-Y_0_year3[test_index_X5_1])*y_task1_train[test_index_X5_1]*num_patients_year3[test_index_X5_1])/D_est - sum(p_score[test_index_X5_1]*(1-y_task1_train[test_index_X5_1])*(y_task2_year3[test_index_X5_1]-Y_0_year3[test_index_X5_1])*num_patients_year3[test_index_X5_1]/(D_est*(1-p_score[test_index_X5_1])))
    b_2=sum((y_task2_year4[test_index_X5_1]-Y_0_year4[test_index_X5_1])*y_task1_train[test_index_X5_1]*num_patients_year4[test_index_X5_1])/D_est - sum(p_score[test_index_X5_1]*(1-y_task1_train[test_index_X5_1])*(y_task2_year4[test_index_X5_1]-Y_0_year4[test_index_X5_1])*num_patients_year4[test_index_X5_1]/(D_est*(1-p_score[test_index_X5_1])))
    b=b_1+b_2
    a_X5_1[j]=sum(y_task1_train[test_index_X5_1]*num_patients_year3[test_index_X5_1]+y_task1_train[test_index_X5_1]*num_patients_year4[test_index_X5_1])/D_est
    theta_X5_1[j]=solve(a_X5_1[j],b)
    b_el_1_15=(y_task2_year3[test_index_X5_1]-Y_0_year3[test_index_X5_1])*y_task1_train[test_index_X5_1]/D_est - p_score[test_index_X5_1]*(1-y_task1_train[test_index_X5_1])*(y_task2_year3[test_index_X5_1]-Y_0_year3[test_index_X5_1])/(D_est*(1-p_score[test_index_X5_1]))
    b_el_2_15=(y_task2_year4[test_index_X5_1]-Y_0_year4[test_index_X5_1])*y_task1_train[test_index_X5_1]/D_est - p_score[test_index_X5_1]*(1-y_task1_train[test_index_X5_1])*(y_task2_year4[test_index_X5_1]-Y_0_year4[test_index_X5_1])/(D_est*(1-p_score[test_index_X5_1]))
    a_el_15=y_task1_train[test_index_X5_1]/D_est
    psi_squared_sum_X5_1[[j]]=function(x) sum((b_el_1_15-a_el_15*x)^2*num_patients_year3[test_index_X5_1])+sum((b_el_2_15-a_el_15*x)^2*num_patients_year4[test_index_X5_1])
    
  }  
  for (j in 1:K1){
    train_index_overall=N_overall[cv_folds_train_overall[[j]]]
    test_index_overall=N_overall[-cv_folds_train_overall[[j]]]
    #OVERALL:
    N_test_overall[j]=sum(num_patients_year3[test_index_overall]+num_patients_year4[test_index_overall])
    N_train=sum(num_patients_year3[train_index_overall]+num_patients_year4[train_index_overall])
    D_est=sum(y_task1_train[train_index_overall]*num_patients_year3[train_index_overall]+y_task1_train[train_index_overall]*num_patients_year4[train_index_overall])/N_train
    a_overall[j]=sum(y_task1_train[test_index_overall]*num_patients_year3[test_index_overall]+y_task1_train[test_index_overall]*num_patients_year4[test_index_overall])/D_est
    b_el_1_1=(y_task2_year3[test_index_overall]-Y_0_year3[test_index_overall])*y_task1_train[test_index_overall]/D_est - p_score[test_index_overall]*(1-y_task1_train[test_index_overall])*(y_task2_year3[test_index_overall]-Y_0_year3[test_index_overall])/(D_est*(1-p_score[test_index_overall]))
    b_el_2_1=(y_task2_year4[test_index_overall]-Y_0_year4[test_index_overall])*y_task1_train[test_index_overall]/D_est - p_score[test_index_overall]*(1-y_task1_train[test_index_overall])*(y_task2_year4[test_index_overall]-Y_0_year4[test_index_overall])/(D_est*(1-p_score[test_index_overall]))
    a_el_1=y_task1_train[test_index_overall]/D_est
    psi_squared_sum_overall[[j]]=function(x) sum((b_el_1_1-a_el_1*x)^2*num_patients_year3[test_index_overall])+sum((b_el_2_1-a_el_1*x)^2*num_patients_year4[test_index_overall])
  }
  for (j in 1:K3){
    train_index_X4_C=N_X4_C[cv_folds_train_X4_C[[j]]]
    test_index_X4_C=N_X4_C[-cv_folds_train_X4_C[[j]]]
    #X4_C:
    N_test_X4_C[j]=sum(num_patients_year3[test_index_X4_C]+num_patients_year4[test_index_X4_C])
    N_train=sum(num_patients_year3[train_index_X4_C]+num_patients_year4[train_index_X4_C])
    D_est=sum(y_task1_train[train_index_X4_C]*num_patients_year3[train_index_X4_C]+y_task1_train[train_index_X4_C]*num_patients_year4[train_index_X4_C])/N_train
    b_1=sum((y_task2_year3[test_index_X4_C]-Y_0_year3[test_index_X4_C])*y_task1_train[test_index_X4_C]*num_patients_year3[test_index_X4_C])/D_est - sum(p_score[test_index_X4_C]*(1-y_task1_train[test_index_X4_C])*(y_task2_year3[test_index_X4_C]-Y_0_year3[test_index_X4_C])*num_patients_year3[test_index_X4_C]/(D_est*(1-p_score[test_index_X4_C])))
    b_2=sum((y_task2_year4[test_index_X4_C]-Y_0_year4[test_index_X4_C])*y_task1_train[test_index_X4_C]*num_patients_year4[test_index_X4_C])/D_est - sum(p_score[test_index_X4_C]*(1-y_task1_train[test_index_X4_C])*(y_task2_year4[test_index_X4_C]-Y_0_year4[test_index_X4_C])*num_patients_year4[test_index_X4_C]/(D_est*(1-p_score[test_index_X4_C])))
    b=b_1+b_2
    a_X4_C[j]=sum(y_task1_train[test_index_X4_C]*num_patients_year3[test_index_X4_C]+y_task1_train[test_index_X4_C]*num_patients_year4[test_index_X4_C])/D_est
    theta_X4_C[j]=solve(a_X4_C[j],b)
    b_el_1_13=(y_task2_year3[test_index_X4_C]-Y_0_year3[test_index_X4_C])*y_task1_train[test_index_X4_C]/D_est - p_score[test_index_X4_C]*(1-y_task1_train[test_index_X4_C])*(y_task2_year3[test_index_X4_C]-Y_0_year3[test_index_X4_C])/(D_est*(1-p_score[test_index_X4_C]))
    b_el_2_13=(y_task2_year4[test_index_X4_C]-Y_0_year4[test_index_X4_C])*y_task1_train[test_index_X4_C]/D_est - p_score[test_index_X4_C]*(1-y_task1_train[test_index_X4_C])*(y_task2_year4[test_index_X4_C]-Y_0_year4[test_index_X4_C])/(D_est*(1-p_score[test_index_X4_C]))
    a_el_13=y_task1_train[test_index_X4_C]/D_est
    psi_squared_sum_X4_C[[j]]=function(x) sum((b_el_1_13-a_el_13*x)^2*num_patients_year3[test_index_X4_C])+sum((b_el_2_13-a_el_13*x)^2*num_patients_year4[test_index_X4_C])
  }
  #theta_est=sum(theta*N_test)/sum(N_test)
  #SATT_subgroup=theta_est
  #satt_subgroup[i]=SATT_subgroup
  #calculate all 15 SATT statistics (and their corresponding lower90 and upper90 statistics) for dataset i from df_year3 and df_year4:
  #SATT for overall:
  satt_overall=df_record$satt[count+1]
  #SATT for X1_0:
  df_record$satt[count+4]=sum(theta_X1_0*N_test_X1_0)/sum(N_test_X1_0)
  satt_X1_0=df_record$satt[count+4]
  #SATT for X1_1:
  df_record$satt[count+5]=sum(theta_X1_1*N_test_X1_1)/sum(N_test_X1_1)
  satt_X1_1=df_record$satt[count+5]
  #SATT for X2_A:
  df_record$satt[count+6]=sum(theta_X2_A*N_test_X2_A)/sum(N_test_X2_A)
  satt_X2_A=df_record$satt[count+6]
  #SATT for X2_B:
  df_record$satt[count+7]=sum(theta_X2_B*N_test_X2_B)/sum(N_test_X2_B)
  satt_X2_B=df_record$satt[count+7]
  #SATT for X2_C:
  df_record$satt[count+8]=sum(theta_X2_C*N_test_X2_C)/sum(N_test_X2_C)
  satt_X2_C=df_record$satt[count+8]
  #SATT for X3_0:
  df_record$satt[count+9]=sum(theta_X3_0*N_test_X3_0)/sum(N_test_X3_0)
  satt_X3_0=df_record$satt[count+9]
  #SATT for X3_1:
  df_record$satt[count+10]=sum(theta_X3_1*N_test_X3_1)/sum(N_test_X3_1)
  satt_X3_1=df_record$satt[count+10]
  #SATT for X4_A:
  df_record$satt[count+11]=sum(theta_X4_A*N_test_X4_A)/sum(N_test_X4_A)
  satt_X4_A=df_record$satt[count+11]
  #SATT for X4_B:
  df_record$satt[count+12]=sum(theta_X4_B*N_test_X4_B)/sum(N_test_X4_B)
  satt_X4_B=df_record$satt[count+12]
  #SATT for X4_C:
  df_record$satt[count+13]=sum(theta_X4_C*N_test_X4_C)/sum(N_test_X4_C)
  satt_X4_C=df_record$satt[count+13]
  #SATT for X5_0:
  df_record$satt[count+14]=sum(theta_X5_0*N_test_X5_0)/sum(N_test_X5_0)
  satt_X5_0=df_record$satt[count+14]
  #SATT for X5_1:
  df_record$satt[count+15]=sum(theta_X5_1*N_test_X5_1)/sum(N_test_X5_1)
  satt_X5_1=df_record$satt[count+15]
  
  psi_overall=0
  psi_X1_0=0
  psi_X1_1=0
  psi_X2_A=0
  psi_X2_B=0
  psi_X2_C=0
  psi_X3_0=0
  psi_X3_1=0
  psi_X4_A=0
  psi_X4_B=0
  psi_X4_C=0
  psi_X5_0=0
  psi_X5_1=0
  for (h in 1:K2){
    
    psi_X1_0=psi_X1_0+psi_squared_sum_X1_0[[h]](satt_X1_0)
    psi_X1_1=psi_X1_1+psi_squared_sum_X1_1[[h]](satt_X1_1)
    psi_X2_A=psi_X2_A+psi_squared_sum_X2_A[[h]](satt_X2_A)
    psi_X2_B=psi_X2_B+psi_squared_sum_X2_B[[h]](satt_X2_B)
    psi_X2_C=psi_X2_C+psi_squared_sum_X2_C[[h]](satt_X2_C)
    psi_X3_0=psi_X3_0+psi_squared_sum_X3_0[[h]](satt_X3_0)
    psi_X3_1=psi_X3_1+psi_squared_sum_X3_1[[h]](satt_X3_1)
    psi_X4_A=psi_X4_A+psi_squared_sum_X4_A[[h]](satt_X4_A)
    psi_X4_B=psi_X4_B+psi_squared_sum_X4_B[[h]](satt_X4_B)
    
    psi_X5_0=psi_X5_0+psi_squared_sum_X5_0[[h]](satt_X5_0)
    psi_X5_1=psi_X5_1+psi_squared_sum_X5_1[[h]](satt_X5_1)
  }
  for (h in 1:K1){
    psi_overall=psi_overall+psi_squared_sum_overall[[h]](satt_overall)
  }
  for (h in 1:K3){
    psi_X4_C=psi_X4_C+psi_squared_sum_X4_C[[h]](satt_X4_C)
  }
  #calculate CIs:
  #psi_theta=sum(b_year3-a_year3*satt_est)
  sigma_overall=sqrt(psi_overall)/sum(a_overall)
  sigma_X1_0=sqrt(psi_X1_0)/sum(a_X1_0)
  sigma_X1_1=sqrt(psi_X1_1)/sum(a_X1_1)
  sigma_X2_A=sqrt(psi_X2_A)/sum(a_X2_A)
  sigma_X2_B=sqrt(psi_X2_B)/sum(a_X2_B)
  sigma_X2_C=sqrt(psi_X2_C)/sum(a_X2_C)
  sigma_X3_0=sqrt(psi_X3_0)/sum(a_X3_0)
  sigma_X3_1=sqrt(psi_X3_1)/sum(a_X3_1)
  sigma_X4_A=sqrt(psi_X4_A)/sum(a_X4_A)
  sigma_X4_B=sqrt(psi_X4_B)/sum(a_X4_B)
  sigma_X4_C=sqrt(psi_X4_C)/sum(a_X4_C)
  sigma_X5_0=sqrt(psi_X5_0)/sum(a_X5_0)
  sigma_X5_1=sqrt(psi_X5_1)/sum(a_X5_1)
  
  df_record$lower90[count+1]=satt_overall-qnorm(1-alpha/2)*sigma_overall
  df_record$upper90[count+1]=satt_overall+qnorm(1-alpha/2)*sigma_overall
  df_record$lower90[count+4]=satt_X1_0-qnorm(1-alpha/2)*sigma_X1_0
  df_record$upper90[count+4]=satt_X1_0+qnorm(1-alpha/2)*sigma_X1_0
  df_record$lower90[count+5]=satt_X1_1-qnorm(1-alpha/2)*sigma_X1_1
  df_record$upper90[count+5]=satt_X1_1+qnorm(1-alpha/2)*sigma_X1_1
  df_record$lower90[count+6]=satt_X2_A-qnorm(1-alpha/2)*sigma_X2_A
  df_record$upper90[count+6]=satt_X2_A+qnorm(1-alpha/2)*sigma_X2_A
  df_record$lower90[count+7]=satt_X2_B-qnorm(1-alpha/2)*sigma_X2_B
  df_record$upper90[count+7]=satt_X2_B+qnorm(1-alpha/2)*sigma_X2_B
  df_record$lower90[count+8]=satt_X2_C-qnorm(1-alpha/2)*sigma_X2_C
  df_record$upper90[count+8]=satt_X2_C+qnorm(1-alpha/2)*sigma_X2_C
  df_record$lower90[count+9]=satt_X3_0-qnorm(1-alpha/2)*sigma_X3_0
  df_record$upper90[count+9]=satt_X3_0+qnorm(1-alpha/2)*sigma_X3_0
  df_record$lower90[count+10]=satt_X3_1-qnorm(1-alpha/2)*sigma_X3_1
  df_record$upper90[count+10]=satt_X3_1+qnorm(1-alpha/2)*sigma_X3_1
  df_record$lower90[count+11]=satt_X4_A-qnorm(1-alpha/2)*sigma_X4_A
  df_record$upper90[count+11]=satt_X4_A+qnorm(1-alpha/2)*sigma_X4_A
  df_record$lower90[count+12]=satt_X4_B-qnorm(1-alpha/2)*sigma_X4_B
  df_record$upper90[count+12]=satt_X4_B+qnorm(1-alpha/2)*sigma_X4_B
  df_record$lower90[count+13]=satt_X4_C-qnorm(1-alpha/2)*sigma_X4_C
  df_record$upper90[count+13]=satt_X4_C+qnorm(1-alpha/2)*sigma_X4_C
  df_record$lower90[count+14]=satt_X5_0-qnorm(1-alpha/2)*sigma_X5_0
  df_record$upper90[count+14]=satt_X5_0+qnorm(1-alpha/2)*sigma_X5_0
  df_record$lower90[count+15]=satt_X5_1-qnorm(1-alpha/2)*sigma_X5_1
  df_record$upper90[count+15]=satt_X5_1+qnorm(1-alpha/2)*sigma_X5_1
  
  count=count+15
  write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/dml_test_record.csv",row.names = FALSE)  
}
