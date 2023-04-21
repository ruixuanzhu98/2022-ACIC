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
write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/dml_test_record.csv",row.names = FALSE)   
count=0
K=5
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
  X_task2_year3$y_1=(df %>% dplyr::filter(year==1))$Y
  X_task2_year3$y_2=(df %>% dplyr::filter(year==2))$Y
  X_task2_year3$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  X_task2_year3$size_1=(df %>% dplyr::filter(year==1))$n.patients
  X_task2_year3$size_2=(df %>% dplyr::filter(year==2))$n.patients
  num_patients_year3=(df_task2 %>% dplyr::filter(year==3) %>% select(n.patients))$n.patients
  y_task2_year3=df_task2 %>% dplyr::filter(year==3) %>% select(Y)
  y_task2_year3=y_task2_year3$Y
  
  X_task2_year4=df_task2 %>% dplyr::filter(year==4) %>% select(-id.practice,-year,-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_task2_year4$y_1=(df %>% dplyr::filter(year==1))$Y
  X_task2_year4$y_2=(df %>% dplyr::filter(year==2))$Y
  X_task2_year4$y_3=(df %>% dplyr::filter(year==3))$Y
  X_task2_year4$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  X_task2_year4$size_1=(df %>% dplyr::filter(year==1))$n.patients
  X_task2_year4$size_2=(df %>% dplyr::filter(year==2))$n.patients
  num_patients_year4=(df_task2 %>% dplyr::filter(year==4) %>% select(n.patients))$n.patients
  y_task2_year4=df_task2 %>% dplyr::filter(year==4) %>% select(Y)
  y_task2_year4=y_task2_year4$Y
  
 
  N_0=length((df %>% dplyr::filter(year==3))$id.practice)
  

  cv_folds_train=createFolds(1:N_0,k=5,list=TRUE,returnTrain=TRUE)
  theta_overall=vector(mode="numeric",length=K)
  theta_year3=vector(mode="numeric",length=K)
  theta_year4=vector(mode="numeric",length=K)
  a=vector(mode="numeric",length=K)
  a_year3=vector(mode="numeric",length=K)
  a_year4=vector(mode="numeric",length=K)
  b=vector(mode="numeric",length=K)
  b_year3=vector(mode="numeric",length=K)
  b_year4=vector(mode="numeric",length=K)
  N_test=vector(mode="numeric",length=K)
  N_test_3=vector(mode="numeric",length=K)
  N_test_4=vector(mode="numeric",length=K)
  psi_squared_sum_3=vector(mode="list",length=K)
  psi_squared_sum_4=vector(mode="list",length=K)
  psi_squared_sum_overall=vector(mode="list",length=K)
  for (j in 1:K){
    print(i)
    train_index=cv_folds_train[[j]]
    test_index=setdiff(1:N_0,train_index)
    
    X_task1_train=df_task1[train_index,] %>% select(-id.practice,-Z)
    y_task1_train=df_task1[train_index,] %>% select(Z)
    y_task1_train=y_task1_train$Z
    X_task1_test=df_task1[test_index,] %>% select(-id.practice,-Z)
    y_task1_test=df_task1[test_index,] %>% select(Z)
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
    
    X_0_year4=X_task2_year4[test_index,]
    X_0_year4$Z=rep(0,test_len)
    bart_1_year4=BART::gbart(x.train=X_task2_year4[train_index,], y.train=y_task2_year4[train_index],x.test=X_0_year4,type='wbart',ntree=100,ndpost=1000,nskip=500)
    Y_0_year4=bart_1_year4$yhat.test.mean
    
    
    #forming score equation:
    N_test[j]=sum(num_patients_year3[test_index]+num_patients_year4[test_index])
    N_test_3[j]=sum(num_patients_year3[test_index])
    N_test_4[j]=sum(num_patients_year4[test_index])
    D_est_3=sum(y_task1_train*num_patients_year3[train_index])/sum(num_patients_year3[train_index])
    D_est_4=sum(y_task1_train*num_patients_year4[train_index])/sum(num_patients_year4[train_index])
    N_train=sum(num_patients_year3[train_index]+num_patients_year4[train_index])
    D_est=sum(y_task1_train*num_patients_year3[train_index]+y_task1_train*num_patients_year4[train_index])/N_train
    b_year3[j]=sum((y_task2_year3[test_index]-Y_0_year3)*y_task1_test*num_patients_year3[test_index]/D_est_3) - sum(p_score*(1-y_task1_test)*(y_task2_year3[test_index]-Y_0_year3)*num_patients_year3[test_index]/(D_est_3*(1-p_score)))
    b_year4[j]=sum((y_task2_year4[test_index]-Y_0_year4)*y_task1_test*num_patients_year4[test_index])/D_est_4 - sum(p_score*(1-y_task1_test)*(y_task2_year4[test_index]-Y_0_year4)*num_patients_year4[test_index]/(D_est_4*(1-p_score)))
    b_1=sum((y_task2_year3[test_index]-Y_0_year3)*y_task1_test*num_patients_year3[test_index])/D_est - sum(p_score*(1-y_task1_test)*(y_task2_year3[test_index]-Y_0_year3)*num_patients_year3[test_index]/(D_est*(1-p_score)))
    b_2=sum((y_task2_year4[test_index]-Y_0_year4)*y_task1_test*num_patients_year4[test_index])/D_est - sum(p_score*(1-y_task1_test)*(y_task2_year4[test_index]-Y_0_year4)*num_patients_year4[test_index]/(D_est*(1-p_score)))
    b[j]=b_1+b_2
    a[j]=sum(y_task1_test*num_patients_year3[test_index]+y_task1_test*num_patients_year4[test_index])/D_est
    a_year3[j]=sum(y_task1_test*num_patients_year3[test_index]/D_est_3)
    a_year4[j]=sum(y_task1_test*num_patients_year4[test_index]/D_est_4)
    theta_overall[j]=solve(a[j],b[j])
    theta_year3[j]=solve(a_year3[j],b_year3[j])
    theta_year4[j]=solve(a_year4[j],b_year4[j])
    b_el_1=(y_task2_year3[test_index]-Y_0_year3)*y_task1_test/D_est - p_score*(1-y_task1_test)*(y_task2_year3[test_index]-Y_0_year3)/(D_est*(1-p_score))
    b_el_2=(y_task2_year4[test_index]-Y_0_year4)*y_task1_test/D_est - p_score*(1-y_task1_test)*(y_task2_year4[test_index]-Y_0_year4)/(D_est*(1-p_score))
    b_el_3=(y_task2_year3[test_index]-Y_0_year3)*y_task1_test/D_est_3 - p_score*(1-y_task1_test)*(y_task2_year3[test_index]-Y_0_year3)/(D_est_3*(1-p_score))
    b_el_4=(y_task2_year4[test_index]-Y_0_year4)*y_task1_test/D_est_4 - p_score*(1-y_task1_test)*(y_task2_year4[test_index]-Y_0_year4)/(D_est_4*(1-p_score))
    a_el=y_task1_test/D_est
    a_el_3=y_task1_test/D_est_3
    a_el_4=y_task1_test/D_est_4
    psi_squared_sum_overall[[j]]=function(x) sum((b_el_1-a_el*x)^2*num_patients_year3[test_index])+sum((b_el_2-a_el*x)^2*num_patients_year3[test_index])
    psi_squared_sum_3[[j]]=function(x) sum((b_el_3-a_el_3*x)^2*num_patients_year3[test_index])
    psi_squared_sum_4[[j]]=function(x) sum((b_el_4-a_el_4*x)^2*num_patients_year4[test_index])
  }
  print("record")
  #calculate all 15 SATT statistics (and their corresponding lower90 and upper90 statistics) for dataset i from df_year3 and df_year4:
  #SATT for year 3:
  df_record$satt[count+2]=sum(theta_year3*N_test_3)/sum(N_test_3)
  satt_3=df_record$satt[count+2]
  #SATT for year 4:
  df_record$satt[count+3]=sum(theta_year4*N_test_4)/sum(N_test_4)
  satt_4=df_record$satt[count+3]
  #SATT for overall:
  df_record$satt[count+1]=sum(theta_overall*N_test)/sum(N_test)
  satt_overall=df_record$satt[count+1]
  psi_3=0
  psi_4=0
  psi_overall=0
  for (h in 1:K){
    psi_3=psi_3+psi_squared_sum_3[[h]](satt_3)
    psi_4=psi_4+psi_squared_sum_4[[h]](satt_4)
    psi_overall=psi_overall+psi_squared_sum_overall[[h]](satt_overall)
  }
  #calculate CIs:
  #psi_theta=sum(b_year3-a_year3*satt_est)
  sigma_3=sqrt(psi_3)/sum(a_year3)
  sigma_4=sqrt(psi_4)/sum(a_year4)
  sigma_overall=sqrt(psi_overall)/sum(a)
  df_record$lower90[count+2]=satt_3-qnorm(1-alpha/2)*sigma_3
  df_record$upper90[count+2]=satt_3+qnorm(1-alpha/2)*sigma_3
  df_record$lower90[count+3]=satt_4-qnorm(1-alpha/2)*sigma_4
  df_record$upper90[count+3]=satt_4+qnorm(1-alpha/2)*sigma_4
  df_record$lower90[count+1]=satt_overall-qnorm(1-alpha/2)*sigma_overall
  df_record$upper90[count+1]=satt_overall+qnorm(1-alpha/2)*sigma_overall
  
  count=count+15
  write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/dml_test_record.csv",row.names = FALSE)  
}
