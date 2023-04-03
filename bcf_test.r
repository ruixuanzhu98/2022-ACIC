library(bcf)
library(tidyverse)
library(BayesTree)


#create record data frame:
dataset.num=rep(formatC(1:3400, width=4, flag = "0"),each=15)
variable=rep(c("Overall","Overall","Overall","X1","X1","X2","X2","X2","X3","X3","X4","X4","X4","X5","X5"),3400)
level=rep(c(NA,NA,NA,0,1,"A","B","C",0,1,"A","B","C",0,1),3400)
year=rep(c(NA,3,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),3400)
satt=vector(mode = "numeric", length = 51000)
lower90=vector(mode = "numeric", length = 51000)
upper90=vector(mode = "numeric", length = 51000)
df_record=data.frame(dataset.num,variable,level,year,satt,lower90,upper90)
write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/bcf_new_record.csv",row.names = FALSE)   

#compute Y_1, Y_0 and SATT values:
count=0
for (i in 1:3400){
  print(i)
  path=str_interp("/Users/piglet/Desktop/track2_20220404/merged/acic_practice_${i}.csv")
  df=read.csv(path)
  df_record=read.csv("/Users/piglet/Desktop/track2_20220404/bcf_new_record.csv")
  
  #calculate for year3:
  df_year3=df %>% dplyr::filter(year==3) %>% select(-id.practice,-year,-post)
  df_year3$y_1=(df %>% dplyr::filter(year==1))$Y
  df_year3$y_2=(df %>% dplyr::filter(year==2))$Y
  df_year3$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_year3$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_year3$size_2=(df %>% dplyr::filter(year==2))$n.patients
  
  df_year3$X2=as.factor(df_year3$X2)
  df_year3$X4=as.factor(df_year3$X4)

  X_con=df_year3 %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg,-p_score,-Z)
  X_mod=df_year3 %>% select(X1,X2,X3,X4,X5,X6,X7,X8,X9)
  #X_mod=X_con
  y=df_year3 %>% select(Y)
  y=y$Y
  N_3=sum(df_year3$n.patients[df_year3$Z==1])
  X_con=BayesTree::makeind(X_con)
  X_mod=BayesTree::makeind(X_mod)
  
  bcf_year3=bcf(y, df_year3$Z, X_con, X_mod, df_year3$p_score, nburn=500, nsim=1000,include_pi='both')
  #print(dim(bcf_year3$tau))
  #1000*500
  treated_index=which(df_year3$Z==1,arr.ind = TRUE)
  tau_year3=bcf_year3$tau[,treated_index]
  tau_mean_year3=apply(bcf_year3$tau,2,mean)
  tau_mean_tr_year3=tau_mean_year3[df_year3$Z==1]
  rm(X_con,X_mod,y)
  
  #calculate for year4:
  df_year4=df %>% dplyr::filter(year==4) %>% select(-id.practice,-year,-post)
  df_year4$y_1=(df %>% dplyr::filter(year==1))$Y
  df_year4$y_2=(df %>% dplyr::filter(year==2))$Y
  df_year4$y_3=(df %>% dplyr::filter(year==3))$Y
  df_year4$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_year4$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_year4$size_2=(df %>% dplyr::filter(year==2))$n.patients
  
  df_year4$X2=as.factor(df_year4$X2)
  df_year4$X4=as.factor(df_year4$X4)
  
  X_con=df_year4 %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg,-p_score,-Z)
  X_mod=df_year4 %>% select(X1,X2,X3,X4,X5,X6,X7,X8,X9)
  #X_mod=X_con
  y=df_year4 %>% select(Y)
  y=y$Y
  N_4=sum(df_year4$n.patients[df_year4$Z==1])
  X_con=BayesTree::makeind(X_con)
  X_mod=BayesTree::makeind(X_mod)
  
  bcf_year4=bcf(y, df_year4$Z, X_con, X_mod, df_year4$p_score, nburn=500, nsim=1000,include_pi='both')
  #print(dim(bcf_year3$tau))
  #1000*500
  treated_index=which(df_year4$Z==1,arr.ind = TRUE)
  tau_year4=bcf_year4$tau[,treated_index]
  tau_mean_year4=apply(bcf_year4$tau,2,mean)
  tau_mean_tr_year4=tau_mean_year4[df_year4$Z==1]
  rm(X_con,X_mod,y)

  
  #prepare to calculate confidence interval:
  satt_year3=vector(mode = "numeric", length = 1000)
  satt_year4=vector(mode = "numeric", length = 1000)
  satt_overall=vector(mode = "numeric", length = 1000)
  satt_X1_0=vector(mode = "numeric", length = 1000) 
  satt_X1_1=vector(mode = "numeric", length = 1000)
  satt_X2_A=vector(mode = "numeric", length = 1000)
  satt_X2_B=vector(mode = "numeric", length = 1000)
  satt_X2_C=vector(mode = "numeric", length = 1000)
  satt_X3_0=vector(mode = "numeric", length = 1000) 
  satt_X3_1=vector(mode = "numeric", length = 1000)
  satt_X4_A=vector(mode = "numeric", length = 1000)
  satt_X4_B=vector(mode = "numeric", length = 1000)
  satt_X4_C=vector(mode = "numeric", length = 1000)
  satt_X5_0=vector(mode = "numeric", length = 1000) 
  satt_X5_1=vector(mode = "numeric", length = 1000)
  num_tr_year3=df_year3$n.patients[df_year3$Z==1]
  num_tr_year4=df_year4$n.patients[df_year4$Z==1]
  
  
  df_year3=df_year3 %>% dplyr::filter(Z==1)
  df_year4=df_year4 %>% dplyr::filter(Z==1)
  
  for (j in 1:1000){
      tau_j_year3=tau_year3[j,]
      tau_j_year4=tau_year4[j,]
      satt_year3[j]=sum(tau_j_year3*num_tr_year3)/N_3
      satt_year4[j]=sum(tau_j_year4*num_tr_year4)/N_4
      satt_overall[j]=(satt_year3[j]*N_3+satt_year4[j]*N_4)/(N_3+N_4)
      satt_X1_0[j]=(sum(tau_j_year3[df_year3$X1==0]*num_tr_year3[df_year3$X1==0])+sum(tau_j_year4[df_year4$X1==0]*num_tr_year4[df_year4$X1==0]))/
                   (sum(num_tr_year3[df_year3$X1==0])+sum(num_tr_year4[df_year4$X1==0]))
      
      satt_X1_1[j]=(sum(tau_j_year3[df_year3$X1==1]*num_tr_year3[df_year3$X1==1])+sum(tau_j_year4[df_year4$X1==1]*num_tr_year4[df_year4$X1==1]))/
                   (sum(num_tr_year3[df_year3$X1==1])+sum(num_tr_year4[df_year4$X1==1]))
      
      satt_X2_A[j]=(sum(tau_j_year3[df_year3$X2=="A"]*num_tr_year3[df_year3$X2=="A"])+sum(tau_j_year4[df_year4$X2=="A"]*num_tr_year4[df_year4$X2=="A"]))/
                   (sum(num_tr_year3[df_year3$X2=="A"])+sum(num_tr_year4[df_year4$X2=="A"]))
      
      satt_X2_B[j]=(sum(tau_j_year3[df_year3$X2=="B"]*num_tr_year3[df_year3$X2=="B"])+sum(tau_j_year4[df_year4$X2=="B"]*num_tr_year4[df_year4$X2=="B"]))/
                   (sum(num_tr_year3[df_year3$X2=="B"])+sum(num_tr_year4[df_year4$X2=="B"]))
      
      satt_X2_C[j]=(sum(tau_j_year3[df_year3$X2=="C"]*num_tr_year3[df_year3$X2=="C"])+sum(tau_j_year4[df_year4$X2=="C"]*num_tr_year4[df_year4$X2=="C"]))/
                   (sum(num_tr_year3[df_year3$X2=="C"])+sum(num_tr_year4[df_year4$X2=="C"]))                                                      
      
      satt_X3_0[j]=(sum(tau_j_year3[df_year3$X3==0]*num_tr_year3[df_year3$X3==0])+sum(tau_j_year4[df_year4$X3==0]*num_tr_year4[df_year4$X3==0]))/
                   (sum(num_tr_year3[df_year3$X3==0])+sum(num_tr_year4[df_year4$X3==0]))
      
      satt_X3_1[j]=(sum(tau_j_year3[df_year3$X3==1]*num_tr_year3[df_year3$X3==1])+sum(tau_j_year4[df_year4$X3==1]*num_tr_year4[df_year4$X3==1]))/
                   (sum(num_tr_year3[df_year3$X3==1])+sum(num_tr_year4[df_year4$X3==1]))
      
      satt_X4_A[j]=(sum(tau_j_year3[df_year3$X4=="A"]*num_tr_year3[df_year3$X4=="A"])+sum(tau_j_year4[df_year4$X4=="A"]*num_tr_year4[df_year4$X4=="A"]))/
                   (sum(num_tr_year3[df_year3$X4=="A"])+sum(num_tr_year4[df_year4$X4=="A"]))
      
      
      satt_X4_B[j]=(sum(tau_j_year3[df_year3$X4=="B"]*num_tr_year3[df_year3$X4=="B"])+sum(tau_j_year4[df_year4$X4=="B"]*num_tr_year4[df_year4$X4=="B"]))/
                   (sum(num_tr_year3[df_year3$X4=="B"])+sum(num_tr_year4[df_year4$X4=="B"]))
      
      satt_X4_C[j]=(sum(tau_j_year3[df_year3$X4=="C"]*num_tr_year3[df_year3$X4=="C"])+sum(tau_j_year4[df_year4$X4=="C"]*num_tr_year4[df_year4$X4=="C"]))/
                   (sum(num_tr_year3[df_year3$X4=="C"])+sum(num_tr_year4[df_year4$X4=="C"]))
      
      
      satt_X5_0[j]=(sum(tau_j_year3[df_year3$X5==0]*num_tr_year3[df_year3$X5==0])+sum(tau_j_year4[df_year4$X5==0]*num_tr_year4[df_year4$X5==0]))/
                   (sum(num_tr_year3[df_year3$X5==0])+sum(num_tr_year4[df_year4$X5==0]))
      
      
      satt_X5_1[j]=(sum(tau_j_year3[df_year3$X5==1]*num_tr_year3[df_year3$X5==1])+sum(tau_j_year4[df_year4$X5==1]*num_tr_year4[df_year4$X5==1]))/
                   (sum(num_tr_year3[df_year3$X5==1])+sum(num_tr_year4[df_year4$X5==1]))
  }
  
  
  #calculate all 15 SATT statistics (and their corresponding lower90 and upper90 statistics) for dataset i from df_year3 and df_year4:
  #SATT for year 3:
  df_record$satt[count+2]=sum(tau_mean_tr_year3*num_tr_year3)/N_3
  
  #satt_year3=unlist(satt_year3)
  df_record$lower90[count+2]=quantile(satt_year3,probs=0.05)
  df_record$upper90[count+2]=quantile(satt_year3,probs=0.95)
  
  #SATT for year 4:
  df_record$satt[count+3]=sum(tau_mean_tr_year4*num_tr_year4)/N_4
  
  #satt_year4=unlist(satt_year4)
  df_record$lower90[count+3]=quantile(satt_year4,probs=0.05)
  df_record$upper90[count+3]=quantile(satt_year4,probs=0.95)
  
  
  #SATT for overall:
  df_record$satt[count+1]=(sum(tau_mean_tr_year3*num_tr_year3)+sum(tau_mean_tr_year4*num_tr_year4))/(N_3+N_4)
  
  #satt_year3=unlist(satt_overall)
  df_record$lower90[count+1]=quantile(satt_overall,probs=0.05)
  df_record$upper90[count+1]=quantile(satt_overall,probs=0.95)
  
  
  #SATT for subgroup X1:
  #X1=0
  df_record$satt[count+4]=(sum(tau_mean_tr_year3[df_year3$X1==0]*num_tr_year3[df_year3$X1==0])+sum(tau_mean_tr_year4[df_year4$X1==0]*num_tr_year4[df_year4$X1==0]))/
                          (sum(num_tr_year3[df_year3$X1==0])+sum(num_tr_year4[df_year4$X1==0]))
  
  #satt_X1_0=unlist(satt_X1_0)
  df_record$lower90[count+4]=quantile(satt_X1_0,probs=0.05)
  df_record$upper90[count+4]=quantile(satt_X1_0,probs=0.95)
  
  
  #X1=1
  df_record$satt[count+5]=(sum(tau_mean_tr_year3[df_year3$X1==1]*num_tr_year3[df_year3$X1==1])+sum(tau_mean_tr_year4[df_year4$X1==1]*num_tr_year4[df_year4$X1==1]))/
                          (sum(num_tr_year3[df_year3$X1==1])+sum(num_tr_year4[df_year4$X1==1]))
  
  #satt_X1_1=unlist(satt_X1_1)
  df_record$lower90[count+5]=quantile(satt_X1_1,probs=0.05)
  df_record$upper90[count+5]=quantile(satt_X1_1,probs=0.95)
  
  
  #SATT for subgroup X2:
  #X2=A
  df_record$satt[count+6]=(sum(tau_mean_tr_year3[df_year3$X2=="A"]*num_tr_year3[df_year3$X2=="A"])+sum(tau_mean_tr_year4[df_year4$X2=="A"]*num_tr_year4[df_year4$X2=="A"]))/
                          (sum(num_tr_year3[df_year3$X2=="A"])+sum(num_tr_year4[df_year4$X2=="A"]))
  
  #satt_X2_A=unlist(satt_X2_A)
  df_record$lower90[count+6]=quantile(satt_X2_A,probs=0.05)
  df_record$upper90[count+6]=quantile(satt_X2_A,probs=0.95)
  
  #X2=B
  df_record$satt[count+7]=(sum(tau_mean_tr_year3[df_year3$X2=="B"]*num_tr_year3[df_year3$X2=="B"])+sum(tau_mean_tr_year4[df_year4$X2=="B"]*num_tr_year4[df_year4$X2=="B"]))/
                          (sum(num_tr_year3[df_year3$X2=="B"])+sum(num_tr_year4[df_year4$X2=="B"]))
  
  #satt_X2_B=unlist(satt_X2_B)
  df_record$lower90[count+7]=quantile(satt_X2_B,probs=0.05)
  df_record$upper90[count+7]=quantile(satt_X2_B,probs=0.95)
  
  
  #X2=C
  df_record$satt[count+8]=(sum(tau_mean_tr_year3[df_year3$X2=="C"]*num_tr_year3[df_year3$X2=="C"])+sum(tau_mean_tr_year4[df_year4$X2=="C"]*num_tr_year4[df_year4$X2=="C"]))/
                          (sum(num_tr_year3[df_year3$X2=="C"])+sum(num_tr_year4[df_year4$X2=="C"]))
  
  #satt_X2_C=unlist(satt_X2_C)
  df_record$lower90[count+8]=quantile(satt_X2_C,probs=0.05)
  df_record$upper90[count+8]=quantile(satt_X2_C,probs=0.95)
  
  
  
  #SATT for subgroup X3:
  #X3=0
  df_record$satt[count+9]=(sum(tau_mean_tr_year3[df_year3$X3==0]*num_tr_year3[df_year3$X3==0])+sum(tau_mean_tr_year4[df_year4$X3==0]*num_tr_year4[df_year4$X3==0]))/
                          (sum(num_tr_year3[df_year3$X3==0])+sum(num_tr_year4[df_year4$X3==0]))
  
  #satt_X3_0=unlist(satt_X3_0)
  df_record$lower90[count+9]=quantile(satt_X3_0,probs=0.05)
  df_record$upper90[count+9]=quantile(satt_X3_0,probs=0.95)
  
  
  #X3=1
  df_record$satt[count+10]=(sum(tau_mean_tr_year3[df_year3$X3==1]*num_tr_year3[df_year3$X3==1])+sum(tau_mean_tr_year4[df_year4$X3==1]*num_tr_year4[df_year4$X3==1]))/
                           (sum(num_tr_year3[df_year3$X3==1])+sum(num_tr_year4[df_year4$X3==1]))
  
  #satt_X3_1=unlist(satt_X3_1)
  df_record$lower90[count+10]=quantile(satt_X3_1,probs=0.05)
  df_record$upper90[count+10]=quantile(satt_X3_1,probs=0.95)
  
  
  #SATT for subgroup X4:
  #X4=A
  df_record$satt[count+11]=(sum(tau_mean_tr_year3[df_year3$X4=="A"]*num_tr_year3[df_year3$X4=="A"])+sum(tau_mean_tr_year4[df_year4$X4=="A"]*num_tr_year4[df_year4$X4=="A"]))/
                           (sum(num_tr_year3[df_year3$X4=="A"])+sum(num_tr_year4[df_year4$X4=="A"]))
  
  #satt_X4_A=unlist(satt_X4_A)
  df_record$lower90[count+11]=quantile(satt_X4_A,probs=0.05)
  df_record$upper90[count+11]=quantile(satt_X4_A,probs=0.95)
  
  #X4=B
  df_record$satt[count+12]=(sum(tau_mean_tr_year3[df_year3$X4=="B"]*num_tr_year3[df_year3$X4=="B"])+sum(tau_mean_tr_year4[df_year4$X4=="B"]*num_tr_year4[df_year4$X4=="B"]))/
                           (sum(num_tr_year3[df_year3$X4=="B"])+sum(num_tr_year4[df_year4$X4=="B"]))
  
  #satt_X4_B=unlist(satt_X4_B)
  df_record$lower90[count+12]=quantile(satt_X4_B,probs=0.05)
  df_record$upper90[count+12]=quantile(satt_X4_B,probs=0.95)
  
  
  #X4=C
  df_record$satt[count+13]=(sum(tau_mean_tr_year3[df_year3$X4=="C"]*num_tr_year3[df_year3$X4=="C"])+sum(tau_mean_tr_year4[df_year4$X4=="C"]*num_tr_year4[df_year4$X4=="C"]))/
                           (sum(num_tr_year3[df_year3$X4=="C"])+sum(num_tr_year4[df_year4$X4=="C"]))
  
  #satt_X4_C=unlist(satt_X4_C)
  df_record$lower90[count+13]=quantile(satt_X4_C,probs=0.05)
  df_record$upper90[count+13]=quantile(satt_X4_C,probs=0.95)
  
  
  #SATT for subgroup X5:
  #X5=0
  df_record$satt[count+14]=(sum(tau_mean_tr_year3[df_year3$X5==0]*num_tr_year3[df_year3$X5==0])+sum(tau_mean_tr_year4[df_year4$X5==0]*num_tr_year4[df_year4$X5==0]))/
                           (sum(num_tr_year3[df_year3$X5==0])+sum(num_tr_year4[df_year4$X5==0]))
  
  #satt_X5_0=unlist(satt_X5_0)
  df_record$lower90[count+14]=quantile(satt_X5_0,probs=0.05)
  df_record$upper90[count+14]=quantile(satt_X5_0,probs=0.95)
  
  
  #X5=1
  df_record$satt[count+15]=(sum(tau_mean_tr_year3[df_year3$X5==1]*num_tr_year3[df_year3$X5==1])+sum(tau_mean_tr_year4[df_year4$X5==1]*num_tr_year4[df_year4$X5==1]))/
                           (sum(num_tr_year3[df_year3$X5==1])+sum(num_tr_year4[df_year4$X5==1]))
  
  #satt_X5_1=unlist(satt_X5_1)
  df_record$lower90[count+15]=quantile(satt_X5_1,probs=0.05)
  df_record$upper90[count+15]=quantile(satt_X5_1,probs=0.95)
  
  
  count=count+15
  rm(bcf_year3,bcf_year4)
  write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/bcf_new_record.csv",row.names = FALSE)  
}

  