library(BART)
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
write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/nops_record.csv",row.names = FALSE)   

#compute Y_1, Y_0 and SATT vaules:
count=0
for (i in 1:3400){
  print(i)
  path=str_interp("/Users/piglet/Desktop/track2_20220404/merged/acic_practice_${i}.csv")
  df=read.csv(path)
  df_record=read.csv("/Users/piglet/Desktop/track2_20220404/nops_record.csv")
  
  #calculate for year3:
  df_year3=df %>% dplyr::filter(year==3) %>% select(-id.practice,-year,-post)
  df_year3$y_1=(df %>% dplyr::filter(year==1))$Y
  df_year3$y_2=(df %>% dplyr::filter(year==2))$Y
  df_year3$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_year3$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_year3$size_2=(df %>% dplyr::filter(year==2))$n.patients
  
  df_year3$X2=as.factor(df_year3$X2)
  df_year3$X4=as.factor(df_year3$X4)
  #df_year3=BayesTree::makeind(df_year3)
  #df_year3=as.data.frame(df_year3)
  
  X=df_year3 %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  y=df_year3 %>% select(Y)
  y=y$Y
  #df_treated=df_year3 %>% dplyr::filter(Z==1)
  X_1=df_year3 %>% dplyr::filter(Z==1) %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_0=df_year3 %>% dplyr::filter(Z==1) %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_0$Z=1-X_0$Z
  N_3=sum((df_year3 %>% dplyr::filter(Z==1))$n.patients)
  X_combined=rbind(X_1,X_0)
  bart_year3=BART::wbart(x.train=X, y.train=y,x.test=X_combined,ntree=100,ndpost=1000,nskip=500)
  Y_year3_1=bart_year3$yhat.test.mean[1:nrow(X_1)]
  Y_year3_0=bart_year3$yhat.test.mean[(nrow(X_1)+1):(2*nrow(X_1))]
  df_year3=df_year3 %>% dplyr::filter(Z==1)
  df_year3$ATE=Y_year3_1-Y_year3_0
  rm(X,X_combined,X_1,X_0,y)
  
  #calculate for year4:
  df_year4=df %>% dplyr::filter(year==4) %>% select(-id.practice,-year,-post,-p_score)
  df_year4$y_1=(df %>% dplyr::filter(year==1))$Y
  df_year4$y_2=(df %>% dplyr::filter(year==2))$Y
  df_year4$y_3=(df %>% dplyr::filter(year==3))$Y
  df_year4$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_year4$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_year4$size_2=(df %>% dplyr::filter(year==2))$n.patients
  
  df_year4$X2=as.factor(df_year4$X2)
  df_year4$X4=as.factor(df_year4$X4)
  #df_year4=BayesTree::makeind(df_year4)
  #df_year4=as.data.frame(df_year4)
  
  X=df_year4 %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  y=df_year4 %>% select(Y)
  y=y$Y
  #df_treated=df_year4 %>% dplyr::filter(Z==1)
  X_1=df_year4 %>% dplyr::filter(Z==1) %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_0=df_year4 %>% dplyr::filter(Z==1) %>% select(-Y,-n.patients,-V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  X_0$Z=1-X_0$Z
  N_4=sum((df_year4 %>% dplyr::filter(Z==1))$n.patients)
  X_combined=rbind(X_1,X_0)
  bart_year4=BART::wbart(x.train=X, y.train=y,x.test=X_combined,ntree=100,ndpost=1000,nskip=500)
  Y_year4_1=bart_year4$yhat.test.mean[1:nrow(X_1)]
  Y_year4_0=bart_year4$yhat.test.mean[(nrow(X_1)+1):(2*nrow(X_1))]
  df_year4=df_year4 %>% dplyr::filter(Z==1)
  df_year4$ATE=Y_year4_1-Y_year4_0
  rm(X,X_combined,X_1,X_0,y)
  
  print("ok")
  start_time=Sys.time()
  #####pre-calculation for lower90 and upper90 statistics:
  num_samples=10
  sigma_posterior_year3=bart_year3$sigma
  fx_posterior_1_year3=bart_year3$yhat.test[,1:nrow(df_year3)]
  fx_posterior_0_year3=bart_year3$yhat.test[,(nrow(df_year3)+1):(2*nrow(df_year3))]
  sigma_posterior_year4=bart_year4$sigma
  fx_posterior_1_year4=bart_year4$yhat.test[,1:nrow(df_year3)]
  fx_posterior_0_year4=bart_year4$yhat.test[,(nrow(df_year3)+1):(2*nrow(df_year3))]
  
  satt_year3=vector(mode = "numeric", length = 1000*num_samples)
  satt_year4=vector(mode = "numeric", length = 1000*num_samples)
  satt_overall=vector(mode = "numeric", length = 1000*num_samples)
  satt_X1_0=vector(mode = "numeric", length = 1000*num_samples) 
  satt_X1_1=vector(mode = "numeric", length = 1000*num_samples)
  satt_X2_A=vector(mode = "numeric", length = 1000*num_samples)
  satt_X2_B=vector(mode = "numeric", length = 1000*num_samples)
  satt_X2_C=vector(mode = "numeric", length = 1000*num_samples)
  satt_X3_0=vector(mode = "numeric", length = 1000*num_samples) 
  satt_X3_1=vector(mode = "numeric", length = 1000*num_samples)
  satt_X4_A=vector(mode = "numeric", length = 1000*num_samples)
  satt_X4_B=vector(mode = "numeric", length = 1000*num_samples)
  satt_X4_C=vector(mode = "numeric", length = 1000*num_samples)
  satt_X5_0=vector(mode = "numeric", length = 1000*num_samples) 
  satt_X5_1=vector(mode = "numeric", length = 1000*num_samples)
  
  for (j in 1:1000){
    for (k in 1:num_samples){
      y1_posterior_year3=rnorm(length(fx_posterior_1_year3[j,]),mean=fx_posterior_1_year3[j,],sd=sigma_posterior_year3[j])
      y0_posterior_year3=rnorm(length(fx_posterior_0_year3[j,]),mean=fx_posterior_0_year3[j,],sd=sigma_posterior_year3[j])
      y1_posterior_year4=rnorm(length(fx_posterior_1_year4[j,]),mean=fx_posterior_1_year4[j,],sd=sigma_posterior_year4[j])
      y0_posterior_year4=rnorm(length(fx_posterior_0_year4[j,]),mean=fx_posterior_0_year4[j,],sd=sigma_posterior_year4[j])
      satt_year3[(j-1)*num_samples+k]=sum((y1_posterior_year3-y0_posterior_year3)*df_year3$n.patients)/N_3
      satt_year4[(j-1)*num_samples+k]=sum((y1_posterior_year4-y0_posterior_year4)*df_year4$n.patients)/N_4
      satt_overall[(j-1)*num_samples+k]=(satt_year3[(j-1)*num_samples+k]*N_3+satt_year4[(j-1)*num_samples+k]*N_4)/(N_3+N_4)
      satt_X1_0[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X1==0]-y0_posterior_year3[df_year3$X1==0])*df_year3$n.patients[df_year3$X1==0])+
                                     sum((y1_posterior_year4[df_year4$X1==0]-y0_posterior_year4[df_year4$X1==0])*df_year4$n.patients[df_year4$X1==0]))/
                                     (sum(df_year3$n.patients[df_year3$X1==0])+sum(df_year4$n.patients[df_year4$X1==0]))
      
      satt_X1_1[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X1==1]-y0_posterior_year3[df_year3$X1==1])*df_year3$n.patients[df_year3$X1==1])+
                                     sum((y1_posterior_year4[df_year4$X1==1]-y0_posterior_year4[df_year4$X1==1])*df_year4$n.patients[df_year4$X1==1]))/
                                     (sum(df_year3$n.patients[df_year3$X1==1])+sum(df_year4$n.patients[df_year4$X1==1]))
      
      satt_X2_A[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X2=="A"]-y0_posterior_year3[df_year3$X2=="A"])*df_year3$n.patients[df_year3$X2=="A"])+
                                     sum((y1_posterior_year4[df_year4$X2=="A"]-y0_posterior_year4[df_year4$X2=="A"])*df_year4$n.patients[df_year4$X2=="A"]))/
                                     (sum(df_year3$n.patients[df_year3$X2=="A"])+sum(df_year4$n.patients[df_year4$X2=="A"]))
      
      satt_X2_B[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X2=="B"]-y0_posterior_year3[df_year3$X2=="B"])*df_year3$n.patients[df_year3$X2=="B"])+
                                     sum((y1_posterior_year4[df_year4$X2=="B"]-y0_posterior_year4[df_year4$X2=="B"])*df_year4$n.patients[df_year4$X2=="B"]))/
                                     (sum(df_year3$n.patients[df_year3$X2=="B"])+sum(df_year4$n.patients[df_year4$X2=="B"]))
      
      satt_X2_C[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X2=="C"]-y0_posterior_year3[df_year3$X2=="C"])*df_year3$n.patients[df_year3$X2=="C"])+
                                     sum((y1_posterior_year4[df_year4$X2=="C"]-y0_posterior_year4[df_year4$X2=="C"])*df_year4$n.patients[df_year4$X2=="C"]))/
                                     (sum(df_year3$n.patients[df_year3$X2=="C"])+sum(df_year4$n.patients[df_year4$X2=="C"]))                                                             
      
      satt_X3_0[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X3==0]-y0_posterior_year3[df_year3$X3==0])*df_year3$n.patients[df_year3$X3==0])+
                                     sum((y1_posterior_year4[df_year4$X3==0]-y0_posterior_year4[df_year4$X3==0])*df_year4$n.patients[df_year4$X3==0]))/
                                     (sum(df_year3$n.patients[df_year3$X3==0])+sum(df_year4$n.patients[df_year4$X3==0]))
      
      satt_X3_1[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X3==1]-y0_posterior_year3[df_year3$X3==1])*df_year3$n.patients[df_year3$X3==1])+
                                     sum((y1_posterior_year4[df_year4$X3==1]-y0_posterior_year4[df_year4$X3==1])*df_year4$n.patients[df_year4$X3==1]))/
                                     (sum(df_year3$n.patients[df_year3$X3==1])+sum(df_year4$n.patients[df_year4$X3==1]))
      
      satt_X4_A[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X4=="A"]-y0_posterior_year3[df_year3$X4=="A"])*df_year3$n.patients[df_year3$X4=="A"])+
                                     sum((y1_posterior_year4[df_year4$X4=="A"]-y0_posterior_year4[df_year4$X4=="A"])*df_year4$n.patients[df_year4$X4=="A"]))/
                                     (sum(df_year3$n.patients[df_year3$X4=="A"])+sum(df_year4$n.patients[df_year4$X4=="A"]))
      
      
      satt_X4_B[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X4=="B"]-y0_posterior_year3[df_year3$X4=="B"])*df_year3$n.patients[df_year3$X4=="B"])+
                                     sum((y1_posterior_year4[df_year4$X4=="B"]-y0_posterior_year4[df_year4$X4=="B"])*df_year4$n.patients[df_year4$X4=="B"]))/
                                     (sum(df_year3$n.patients[df_year3$X4=="B"])+sum(df_year4$n.patients[df_year4$X4=="B"]))
      
      satt_X4_C[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X4=="C"]-y0_posterior_year3[df_year3$X4=="C"])*df_year3$n.patients[df_year3$X4=="C"])+
                                     sum((y1_posterior_year4[df_year4$X4=="C"]-y0_posterior_year4[df_year4$X4=="C"])*df_year4$n.patients[df_year4$X4=="C"]))/
                                     (sum(df_year3$n.patients[df_year3$X4=="C"])+sum(df_year4$n.patients[df_year4$X4=="C"]))
      
      
      satt_X5_0[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X5==0]-y0_posterior_year3[df_year3$X5==0])*df_year3$n.patients[df_year3$X5==0])+
                                     sum((y1_posterior_year4[df_year4$X5==0]-y0_posterior_year4[df_year4$X5==0])*df_year4$n.patients[df_year4$X5==0]))/
                                     (sum(df_year3$n.patients[df_year3$X5==0])+sum(df_year4$n.patients[df_year4$X5==0]))
      
      
      satt_X5_1[(j-1)*num_samples+k]=(sum((y1_posterior_year3[df_year3$X5==1]-y0_posterior_year3[df_year3$X5==1])*df_year3$n.patients[df_year3$X5==1])+
                                     sum((y1_posterior_year4[df_year4$X5==1]-y0_posterior_year4[df_year4$X5==1])*df_year4$n.patients[df_year4$X5==1]))/
                                     (sum(df_year3$n.patients[df_year3$X5==1])+sum(df_year4$n.patients[df_year4$X5==1]))
    }
  }
  
  
  
  end_time=Sys.time()
  print(end_time-start_time)
  print("ok")
  #calculate all 15 SATT statistics (and their corresponding lower90 and upper90 statistics) for dataset i from df_year3 and df_year4:
  #SATT for year 3:
  df_record$satt[count+2]=sum(df_year3$ATE*df_year3$n.patients)/N_3
  
  #satt_year3=unlist(satt_year3)
  df_record$lower90[count+2]=quantile(satt_year3,probs=0.05)
  df_record$upper90[count+2]=quantile(satt_year3,probs=0.95)
  
  #SATT for year 4:
  df_record$satt[count+3]=sum(df_year4$ATE*df_year4$n.patients)/N_4
  
  #satt_year4=unlist(satt_year4)
  df_record$lower90[count+3]=quantile(satt_year4,probs=0.05)
  df_record$upper90[count+3]=quantile(satt_year4,probs=0.95)
  
  
  #SATT for overall:
  df_record$satt[count+1]=(sum(df_year3$ATE*df_year3$n.patients)+sum(df_year4$ATE*df_year4$n.patients))/(N_3+N_4)
  
  #satt_year3=unlist(satt_overall)
  df_record$lower90[count+1]=quantile(satt_overall,probs=0.05)
  df_record$upper90[count+1]=quantile(satt_overall,probs=0.95)
  
  
  #SATT for subgroup X1:
  #X1=0
  df_record$satt[count+4]=(sum(df_year3$ATE[df_year3$X1==0]*df_year3$n.patients[df_year3$X1==0])+
                          sum(df_year4$ATE[df_year4$X1==0]*df_year4$n.patients[df_year4$X1==0]))/
                          (sum(df_year3$n.patients[df_year3$X1==0])+sum(df_year4$n.patients[df_year4$X1==0]))
  
  #satt_X1_0=unlist(satt_X1_0)
  df_record$lower90[count+4]=quantile(satt_X1_0,probs=0.05)
  df_record$upper90[count+4]=quantile(satt_X1_0,probs=0.95)
  
  
  #X1=1
  df_record$satt[count+5]=(sum(df_year3$ATE[df_year3$X1==1]*df_year3$n.patients[df_year3$X1==1])+
                          sum(df_year4$ATE[df_year4$X1==1]*df_year4$n.patients[df_year4$X1==1]))/
                          (sum(df_year3$n.patients[df_year3$X1==1])+sum(df_year4$n.patients[df_year4$X1==1]))
  
  #satt_X1_1=unlist(satt_X1_1)
  df_record$lower90[count+5]=quantile(satt_X1_1,probs=0.05)
  df_record$upper90[count+5]=quantile(satt_X1_1,probs=0.95)
  
  
  #SATT for subgroup X2:
  #X2=A
  df_record$satt[count+6]=(sum(df_year3$ATE[df_year3$X2=="A"]*df_year3$n.patients[df_year3$X2=="A"])+
                          sum(df_year4$ATE[df_year4$X2=="A"]*df_year4$n.patients[df_year4$X2=="A"]))/
                          (sum(df_year3$n.patients[df_year3$X2=="A"])+sum(df_year4$n.patients[df_year4$X2=="A"]))
  
  #satt_X2_A=unlist(satt_X2_A)
  df_record$lower90[count+6]=quantile(satt_X2_A,probs=0.05)
  df_record$upper90[count+6]=quantile(satt_X2_A,probs=0.95)
  
  #X2=B
  df_record$satt[count+7]=(sum(df_year3$ATE[df_year3$X2=="B"]*df_year3$n.patients[df_year3$X2=="B"])+
                          sum(df_year4$ATE[df_year4$X2=="B"]*df_year4$n.patients[df_year4$X2=="B"]))/
                          (sum(df_year3$n.patients[df_year3$X2=="B"])+sum(df_year4$n.patients[df_year4$X2=="B"]))
  
  #satt_X2_B=unlist(satt_X2_B)
  df_record$lower90[count+7]=quantile(satt_X2_B,probs=0.05)
  df_record$upper90[count+7]=quantile(satt_X2_B,probs=0.95)
  
  
  #X2=C
  df_record$satt[count+8]=(sum(df_year3$ATE[df_year3$X2=="C"]*df_year3$n.patients[df_year3$X2=="C"])+
                          sum(df_year4$ATE[df_year4$X2=="C"]*df_year4$n.patients[df_year4$X2=="C"]))/
                          (sum(df_year3$n.patients[df_year3$X2=="C"])+sum(df_year4$n.patients[df_year4$X2=="C"]))
  
  #satt_X2_C=unlist(satt_X2_C)
  df_record$lower90[count+8]=quantile(satt_X2_C,probs=0.05)
  df_record$upper90[count+8]=quantile(satt_X2_C,probs=0.95)
  
  
  
  #SATT for subgroup X3:
  #X3=0
  df_record$satt[count+9]=(sum(df_year3$ATE[df_year3$X3==0]*df_year3$n.patients[df_year3$X3==0])+
                          sum(df_year4$ATE[df_year4$X3==0]*df_year4$n.patients[df_year4$X3==0]))/
                          (sum(df_year3$n.patients[df_year3$X3==0])+sum(df_year4$n.patients[df_year4$X3==0]))
  
  #satt_X3_0=unlist(satt_X3_0)
  df_record$lower90[count+9]=quantile(satt_X3_0,probs=0.05)
  df_record$upper90[count+9]=quantile(satt_X3_0,probs=0.95)
  
  
  #X3=1
  df_record$satt[count+10]=(sum(df_year3$ATE[df_year3$X3==1]*df_year3$n.patients[df_year3$X3==1])+
                           sum(df_year4$ATE[df_year4$X3==1]*df_year4$n.patients[df_year4$X3==1]))/
                           (sum(df_year3$n.patients[df_year3$X3==1])+sum(df_year4$n.patients[df_year4$X3==1]))
  
  #satt_X3_1=unlist(satt_X3_1)
  df_record$lower90[count+10]=quantile(satt_X3_1,probs=0.05)
  df_record$upper90[count+10]=quantile(satt_X3_1,probs=0.95)
  
  
  #SATT for subgroup X4:
  #X4=A
  df_record$satt[count+11]=(sum(df_year3$ATE[df_year3$X4=="A"]*df_year3$n.patients[df_year3$X4=="A"])+
                           sum(df_year4$ATE[df_year4$X4=="A"]*df_year4$n.patients[df_year4$X4=="A"]))/
                           (sum(df_year3$n.patients[df_year3$X4=="A"])+sum(df_year4$n.patients[df_year4$X4=="A"]))
  
  #satt_X4_A=unlist(satt_X4_A)
  df_record$lower90[count+11]=quantile(satt_X4_A,probs=0.05)
  df_record$upper90[count+11]=quantile(satt_X4_A,probs=0.95)
  
  #X4=B
  df_record$satt[count+12]=(sum(df_year3$ATE[df_year3$X4=="B"]*df_year3$n.patients[df_year3$X4=="B"])+
                           sum(df_year4$ATE[df_year4$X4=="B"]*df_year4$n.patients[df_year4$X4=="B"]))/
                          (sum(df_year3$n.patients[df_year3$X4=="B"])+sum(df_year4$n.patients[df_year4$X4=="B"]))
  
  #satt_X4_B=unlist(satt_X4_B)
  df_record$lower90[count+12]=quantile(satt_X4_B,probs=0.05)
  df_record$upper90[count+12]=quantile(satt_X4_B,probs=0.95)
  
  
  #X4=C
  df_record$satt[count+13]=(sum(df_year3$ATE[df_year3$X4=="C"]*df_year3$n.patients[df_year3$X4=="C"])+
                           sum(df_year4$ATE[df_year4$X4=="C"]*df_year4$n.patients[df_year4$X4=="C"]))/
                           (sum(df_year3$n.patients[df_year3$X4=="C"])+sum(df_year4$n.patients[df_year4$X4=="C"]))
  
  #satt_X4_C=unlist(satt_X4_C)
  df_record$lower90[count+13]=quantile(satt_X4_C,probs=0.05)
  df_record$upper90[count+13]=quantile(satt_X4_C,probs=0.95)
  
  
  #SATT for subgroup X5:
  #X5=0
  df_record$satt[count+14]=(sum(df_year3$ATE[df_year3$X5==0]*df_year3$n.patients[df_year3$X5==0])+
                           sum(df_year4$ATE[df_year4$X5==0]*df_year4$n.patients[df_year4$X5==0]))/
                           (sum(df_year3$n.patients[df_year3$X5==0])+sum(df_year4$n.patients[df_year4$X5==0]))
  
  #satt_X5_0=unlist(satt_X5_0)
  df_record$lower90[count+14]=quantile(satt_X5_0,probs=0.05)
  df_record$upper90[count+14]=quantile(satt_X5_0,probs=0.95)
  
  
  #X5=1
  df_record$satt[count+15]=(sum(df_year3$ATE[df_year3$X5==1]*df_year3$n.patients[df_year3$X5==1])+
                            sum(df_year4$ATE[df_year4$X5==1]*df_year4$n.patients[df_year4$X5==1]))/
                            (sum(df_year3$n.patients[df_year3$X5==1])+sum(df_year4$n.patients[df_year4$X5==1]))
  
  #satt_X5_1=unlist(satt_X5_1)
  df_record$lower90[count+15]=quantile(satt_X5_1,probs=0.05)
  df_record$upper90[count+15]=quantile(satt_X5_1,probs=0.95)
  
  
  count=count+15
  rm(bart_year3,bart_year4)
  write.csv(df_record,"/Users/piglet/Desktop/track2_20220404/nops_record.csv",row.names = FALSE)  
}

  
  
  
  
  
  
  
  
  
  
  


