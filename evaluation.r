library(tidyverse)
#####Evaluation of RMSE:
rmse <- function(y1,y2){
  RMSE=sqrt(mean((y1-y2)^2))
  return(RMSE)
}

ci_coverage <- function(satt_truth,lower_bound,upper_bound){
  within_bound=(satt_truth>=lower_bound) & (satt_truth<=upper_bound)
  CI_coverage=mean(within_bound)
  return(CI_coverage)
}

#####Evalution for BART:
df_record=read.csv("/Users/piglet/Desktop/track2_20220404/dml_test_record.csv")
df_truth=read.csv("/Users/piglet/Desktop/track2_20220404/ACIC_estimand_truths.csv")


satt_year3=(df_record %>% dplyr::filter(year==3))$satt
satt_year4=(df_record %>% dplyr::filter(year==4))$satt
satt_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$satt
satt_year3_truth=(df_truth %>% dplyr::filter(year==3))$SATT
satt_year4_truth=(df_truth %>% dplyr::filter(year==4))$SATT
satt_overall_truth=(df_truth %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$SATT

print(rmse(satt_year3,satt_year3_truth))

print(rmse(satt_year4,satt_year4_truth))

print(rmse(satt_overall,satt_overall_truth))

lower90_year3=(df_record %>% dplyr::filter(year==3))$lower90
upper90_year3=(df_record %>% dplyr::filter(year==3))$upper90
lower90_year4=(df_record %>% dplyr::filter(year==4))$lower90
upper90_year4=(df_record %>% dplyr::filter(year==4))$upper90
lower90_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$lower90
upper90_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$upper90

print(ci_coverage(satt_year3_truth,lower90_year3,upper90_year3))

print(ci_coverage(satt_year4_truth,lower90_year4,upper90_year4))

print(ci_coverage(satt_overall_truth,lower90_overall,upper90_overall))

print(mean(upper90_overall - lower90_overall))

print(ci_coverage(satt_overall_truth,lower90_overall,upper90_overall))
stop("message")

satt_subgroup=(df_record %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$satt
satt_subgroup_truth=(df_truth %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$SATT

print(rmse(satt_subgroup,satt_subgroup_truth))



#####Evaluation of Confidence Interval Coverage:
ci_coverage <- function(satt_truth,lower_bound,upper_bound){
  within_bound=(satt_truth>=lower_bound) & (satt_truth<=upper_bound)
  CI_coverage=mean(within_bound)
  return(CI_coverage)
}

#Interval coverage for SATT Overall:
lower90_year3=(df_record %>% dplyr::filter(year==3))$lower90
upper90_year3=(df_record %>% dplyr::filter(year==3))$upper90
lower90_year4=(df_record %>% dplyr::filter(year==4))$lower90
upper90_year4=(df_record %>% dplyr::filter(year==4))$upper90
lower90_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$lower90
upper90_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$upper90
lower90_subgroup=(df_record %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$lower90
upper90_subgroup=(df_record %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$upper90
print(ci_coverage(satt_year3_truth,lower90_year3,upper90_year3))

print(ci_coverage(satt_year4_truth,lower90_year4,upper90_year4))

print(ci_coverage(satt_overall_truth,lower90_overall,upper90_overall))

print(ci_coverage(satt_subgroup_truth,lower90_subgroup,upper90_subgroup))


stop("message")
#####Evalution for BCF:
df_record=read.csv("/Users/piglet/Desktop/track2_20220404/bcf_test_record.csv")
df_truth=read.csv("/Users/piglet/Desktop/track2_20220404/ACIC_estimand_truths.csv")


satt_year3=(df_record %>% dplyr::filter(year==3))$satt
satt_year4=(df_record %>% dplyr::filter(year==4))$satt
satt_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$satt
satt_year3_truth=(df_truth %>% dplyr::filter(year==3))$SATT
satt_year4_truth=(df_truth %>% dplyr::filter(year==4))$SATT
satt_overall_truth=(df_truth %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$SATT

print(rmse(satt_year3[300:350],satt_year3_truth[300:350]))
#[1] 26.09019
print(rmse(satt_year4[300:350],satt_year4_truth[300:350]))
#[1] 25.14494
stop("message")
print(rmse(satt_overall,satt_overall_truth))
#[1] 22.18361

satt_subgroup=(df_record %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$satt
satt_subgroup_truth=(df_truth %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$SATT

print(rmse(satt_subgroup,satt_subgroup_truth))
#[1] 25.7195


#####Evaluation of Confidence Interval Coverage:
ci_coverage <- function(satt_truth,lower_bound,upper_bound){
  within_bound=(satt_truth>=lower_bound) & (satt_truth<=upper_bound)
  CI_coverage=mean(within_bound)
  return(CI_coverage)
}

#Interval coverage for SATT Overall:
lower90_year3=(df_record %>% dplyr::filter(year==3))$lower90
upper90_year3=(df_record %>% dplyr::filter(year==3))$upper90
lower90_year4=(df_record %>% dplyr::filter(year==4))$lower90
upper90_year4=(df_record %>% dplyr::filter(year==4))$upper90
lower90_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$lower90
upper90_overall=(df_record %>% dplyr::filter(variable=="Overall" & is.na(year)==1))$upper90
lower90_subgroup=(df_record %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$lower90
upper90_subgroup=(df_record %>% dplyr::filter(variable %in% c("X1","X2","X3","X4","X5")))$upper90
print(ci_coverage(satt_year3_truth,lower90_year3,upper90_year3))
#[1] 0.1370588
print(ci_coverage(satt_year4_truth,lower90_year4,upper90_year4))
#[1] 0.1547059
print(ci_coverage(satt_overall_truth,lower90_overall,upper90_overall))
#[1] 0.1323529
print(ci_coverage(satt_subgroup_truth,lower90_subgroup,upper90_subgroup))
#[1] 0.1332598





















