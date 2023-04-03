library(stats)
library(BayesTree)
library(BART)
library(stringr)
library(dplyr)
#library(aws.s3)

for (i in 1:3400){
  print(i)
  path=str_interp("/Users/piglet/Desktop/track2_20220404/merged/acic_practice_${i}.csv")
  #path=str_interp("s3:://acic-data-2022/merged/acic_practice_${i}.csv")
  df=read.csv(path)
  id.practice=seq(1,nrow(df))
  #y_diff=dplyr::filter(df,year==2)$Y-dplyr::filter(df,year==1)$Y
  #df_y_diff=data.frame(id.practice,y_diff)
  #df=left_join(df,df_y_diff,by="id.practice")
  #df_task1=df %>% dplyr::filter(year==1) %>% select(Z,X1,X2,X3,X4,X5,X6,X7,X8,X9,y_diff)
  
  df_task1=df %>% dplyr::filter(year==2) %>% select(-id.practice,-year,-post)
  
  df_task1$y_1=(df %>% dplyr::filter(year==1))$Y
  df_task1$y_2=(df %>% dplyr::filter(year==2))$Y
  df_task1$diff_1=(df %>% dplyr::filter(year==2))$Y-(df %>% dplyr::filter(year==1))$Y
  df_task1$size_1=(df %>% dplyr::filter(year==1))$n.patients
  df_task1$size_2=(df %>% dplyr::filter(year==2))$n.patients
  #df_task1$v1=(df %>% dplyr::filter(year==1))$V1_avg
  #df_task1$v1_2=(df %>% dplyr::filter(year==2))$V1_avg
  #df_task1$v2=(df %>% dplyr::filter(year==1))$V2_avg
  #df_task1$v2_2=(df %>% dplyr::filter(year==2))$V2_avg
  #df_task1$v3=(df %>% dplyr::filter(year==1))$V3_avg
  #df_task1$v3_2=(df %>% dplyr::filter(year==2))$V3_avg
  #df_task1$v4=(df %>% dplyr::filter(year==1))$V4_avg
  #df_task1$v4_2=(df %>% dplyr::filter(year==2))$V4_avg
  #df_task1$v5_a=(df %>% dplyr::filter(year==1))$V5_A_avg
  #df_task1$v5_a_2=(df %>% dplyr::filter(year==2))$V5_A_avg
  #df_task1$v5_b=(df %>% dplyr::filter(year==1))$V5_B_avg
  #df_task1$v5_c_2=(df %>% dplyr::filter(year==2))$V5_B_avg
  #df_task1$v5_c=(df %>% dplyr::filter(year==1))$V5_C_avg
  #df_task1$v5_c_2=(df %>% dplyr::filter(year==2))$V5_C_avg
  
  df_task1$X2=as.factor(df_task1$X2)
  df_task1$X4=as.factor(df_task1$X4)
  df_task1=BayesTree::makeind(df_task1)
  df_task1=as.data.frame(df_task1)
  print(df_task1)
  stop("message")
  X=df_task1 %>% select(-Z,-Y,-n.patients, -V4_avg, -V1_avg,-V2_avg, -V3_avg, -V5_A_avg,-V5_B_avg,-V5_C_avg)
  y=df_task1 %>% select(Z)
  y=y$Z
  #y=as.factor(y)
  
  bart_task1= BART::pbart(x.train=X, y.train=y, ntree=100,ndpost=1000,nskip=250)
  f_x=bart_task1$yhat.train
  p_score_all=apply(f_x,c(1,2),plogis)
  p_score=apply(p_score_all,2,mean)
  #bart_task1=bartMachine(X=X, y=y, num_trees=200)
  #print(investigate_var_importance(bart_task1))
  
  df_p_score=data.frame(id.practice,p_score)
  df=left_join(df,df_p_score,by="id.practice")
  write.csv(df,path,row.names = FALSE)
} 




