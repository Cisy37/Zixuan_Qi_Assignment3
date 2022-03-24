#=================================================================
# Set Directory and Import Data
#=================================================================
####Install####
library(tidyverse)
library(data.table)
library(mlogit)
library(foreign)
library(nnet)
library(stargazer)
library(survival)

rm(list = ls())
wd = "C:/Users/Qi zixuan/Desktop/Econ 613/Assignment 3"
setwd(wd)

datjss <- fread("./Data/datjss.csv", header = T)
datsss <- fread("./Data/datsss.csv", header = T)
datstu <- fread("./Data/datstu_v2.csv", header = T)
datstu <- datstu %>% mutate(V1=as.character(V1))

#=================================================================
# Exercise 1: Basic Statistics
#=================================================================
#### Question 1 ####
num_student <- datstu
nrow(num_student)

num_school <- datsss %>%
  group_by(schoolcode) %>%
  filter(nchar(schoolname) == max(nchar(schoolname))) %>%
  ungroup() %>%
  distinct(schoolcode, .keep_all = T)
nrow(num_school)

num_programs <- datstu %>%
  select(1, 11:16) %>%
  pivot_longer(-V1, names_to = "Choice", values_to = "Programs") %>%
  select(2,3) %>%
  distinct(Programs, .keep_all=T) %>%
  filter(Programs != '')
nrow(num_programs)

#### Question 2 ####
school_data <- datstu %>%
  select(1, 5:10) %>%
  pivot_longer(-V1, names_to = "Choice_sch", values_to = "Schools") %>%
  mutate(V2 = substring(Choice_sch, nchar(Choice_sch), nchar(Choice_sch)),
         V2 = as.character(V2),
         V1 = as.character(V1)) %>%
  select(V1,V2,Choice_sch,Schools)

program_data <- datstu %>%
  select(1, 11:16) %>%
  pivot_longer(-V1, names_to = "Choice_pro", values_to = "Programs") %>%
  mutate(V2 = substring(Choice_pro, nchar(Choice_pro), nchar(Choice_pro)),
         V2 = as.character(V2),
         V1 = as.character(V1)) %>%
  select(V1,V2,Choice_pro,Programs)

choice_data <- school_data %>%
  left_join(program_data, by = c('V1','V2')) %>%
  select(1,3:6) %>%
  filter(Schools!='' & Programs!='') %>%
  mutate(choice = paste(Schools,Programs,sep=',')) %>%
  distinct(choice, .keep_all=T)
nrow(choice_data)
#### Question 3 ####
datstu_choice <- datstu %>%
  select(V1, schoolcode1:schoolcode6, jssdistrict) %>%
  gather(school, schoolcode, schoolcode1:schoolcode6)

num_apply <- datsss %>%
  select(schoolcode, sssdistrict) %>%
  right_join(datstu_choice,by='schoolcode') %>%
  mutate(check=ifelse(jssdistrict == sssdistrict, 1,0)) %>%
  filter(check==1) %>%
  distinct(V1, .keep_all = TRUE)
nrow(num_apply)

#### Question 4 ####
num_admit <- datstu %>%
  select(1:10,18) %>%
  mutate(rankplace=as.numeric(rankplace)) %>%
  pivot_longer('schoolcode1':'schoolcode6', names_to="schoolchoice", values_to="schoolcode") %>%
  filter(!is.na(rankplace) & rankplace != '99') %>%
  group_by(V1) %>%
  filter(row_number() == rankplace) %>%
  ungroup() %>% 
  group_by(schoolcode) %>%
  mutate(num_stu = n()) %>%
  ungroup() %>%
  distinct(schoolcode, .keep_all=T) %>%
  filter(!is.na(schoolcode)) %>%
  select(7,8) %>%
  arrange(schoolcode)
head(num_admit)

#### Question 5 ####
cutoff_score <- datstu %>%
  filter(!is.na(rankplace) & rankplace != '99') %>%
  select(1:10,18) %>%
  mutate(rankplace=as.numeric(rankplace)) %>%
  pivot_longer('schoolcode1':'schoolcode6', names_to="schoolchoice", values_to="schoolcode") %>%
  group_by(V1) %>%
  filter(row_number() == rankplace) %>%
  ungroup() %>% 
  group_by(schoolcode) %>%
  mutate(min_score = min(score)) %>%
  ungroup() %>%
  distinct(schoolcode, .keep_all=T) %>%
  filter(!is.na(schoolcode)) %>%
  select(7,8) %>%
  arrange(schoolcode)
head(cutoff_score)
min(cutoff_score$min_score)

#### Question 6 ####
quality_sch <- datstu %>%
  filter(!is.na(rankplace) & rankplace != '99') %>%
  select(1:10,18) %>%
  mutate(rankplace=as.numeric(rankplace)) %>%
  pivot_longer('schoolcode1':'schoolcode6', names_to="schoolchoice", values_to="schoolcode") %>%
  group_by(V1) %>%
  filter(row_number() == rankplace) %>%
  ungroup() %>% 
  group_by(schoolcode) %>%
  mutate(avg_score = mean(score)) %>%
  ungroup() %>%
  distinct(schoolcode, .keep_all=T) %>%
  filter(!is.na(schoolcode)) %>%
  select(7,8) %>%
  arrange(schoolcode)
head(quality_sch)
mean(quality_sch$avg_score)

#=================================================================
# Exercise 2: Data
#=================================================================
#### Question 1 ####
sch_pro_data <- school_data %>%
  left_join(program_data, by = c('V1','V2')) %>%
  select(1,3:6) %>%
  filter(Schools!='' & Programs!='') %>%
  mutate(choice = paste(Schools,Programs,sep=',')) %>%
  left_join(datstu,by='V1') %>%
  select(1:9,22,23) %>%
  rename(schoolcode='Schools')

sch_dataset <- sch_pro_data %>%
  filter(!is.na(rankplace) & rankplace != '99') %>%
  mutate(rankplace=as.numeric(rankplace)) %>%
  group_by(V1) %>%
  filter(row_number() == rankplace) %>%
  ungroup() %>% 
  group_by(choice) %>%
  mutate(num_stu = n(),
         min_score = min(score),
         avg_score = mean(score)) %>%
  ungroup() %>%
  distinct(choice, .keep_all=T) %>%
  left_join(num_school, by='schoolcode') %>%
  select(6,11:14,17:19) %>%
  rename(cutoff='min_score',quality='avg_score',size='num_stu') %>%
  arrange(choice)
head(sch_dataset)

#=================================================================
# Exercise 3:  Distance
#=================================================================
#### Question 1 ####
stu_home <- datstu %>%
  select(1,17) %>%
  left_join(datjss,by='jssdistrict') %>%
  rename(V1 = 'V1.x',
         jsslong = 'point_x',
         jsslat = 'point_y') %>%
  select(1,2,4,5)

sch_pro_stu_data <- school_data %>%
  left_join(program_data, by = c('V1','V2')) %>%
  select(1,3:6)
sch_stu_dataset <- sch_pro_stu_data %>%
  rename(schoolcode = 'Schools') %>%
  left_join(num_school,by='schoolcode') %>%
  select(1:5,7:10) %>%
  rename(V1 = 'V1.x')
  
stu_dataset <- sch_stu_dataset %>%
  left_join(stu_home, by='V1') %>%
  mutate(distance= sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2 +
                        (69.172*(ssslat-jsslat))^2)) %>%
  select(1,3,5:13)
head(stu_dataset)

#=================================================================
# Exercise 4: Dimensionality Reduction
#=================================================================
#### Question 1 ####
stu_dataset <- stu_dataset %>%
  mutate(scode_rev = substring(schoolcode, 1, 3))
head(data.frame(stu_dataset$schoolcode,stu_dataset$scode_rev))

#### Question 2 ####
stu_dataset <- stu_dataset %>%
  mutate(pgm_rev = ifelse(Programs=='General Arts'|Programs=='Visual Arts','Arts',0),
         pgm_rev = ifelse(Programs=='Home Economics'|Programs=='Business','Economics',pgm_rev),
         pgm_rev = ifelse(Programs=='General Science','Science',pgm_rev),
         pgm_rev = ifelse(Programs!='General Arts' & Programs!='Visual Arts' &
                          Programs!='Home Economics'&Programs!='Business'&
                          Programs!='General Science','Others',pgm_rev))
head(data.frame(stu_dataset$Programs,stu_dataset$pgm_rev))

#### Question 3 ####
stu_dataset <- stu_dataset %>%
  mutate(choice_rev = paste(scode_rev,pgm_rev, sep=' '))
head(data.frame(stu_dataset$scode_rev,stu_dataset$pgm_rev,stu_dataset$choice_rev))

#### Question 4 ####
stu_info <- datstu %>% select(1:4,18)
rev_stu_dataset <- stu_dataset %>%
  filter(scode_rev!='') %>%
  left_join(stu_info, by='V1')

rev_choice_dataset <- rev_stu_dataset %>%
  filter(!is.na(rankplace) & rankplace != '99') %>%
  mutate(rankplace=as.numeric(rankplace)) %>%
  group_by(V1) %>%
  filter(row_number() == rankplace) %>%
  ungroup() %>% 
  group_by(choice_rev) %>%
  mutate(rev_min_score = min(score),
         rev_avg_score = mean(score)) %>%
  ungroup() %>%
  distinct(choice_rev, .keep_all=T) %>%
  select(14,19,20) %>%
  rename(rev_cutoff='rev_min_score',rev_quality='rev_avg_score')
head(rev_choice_dataset)

#### Question 5 & 6 ####
mlogit_data <- rev_stu_dataset %>%
  left_join(rev_choice_dataset, by='choice_rev') %>%
  select(1,14,15,18:20) %>%
  mutate(V1 = as.character(V1)) %>%
  group_by(V1) %>%
  filter(row_number() == 1) %>% # extract the first choice of each student
  ungroup() %>%
  filter(!is.na(score)) %>%
  arrange(desc(score)) %>%
  filter(score >= score[20000]) %>%
  select(V1,choice_rev,rev_quality,score)
head(mlogit_data)

#=================================================================
# Exercise 5: First Model
#=================================================================
####Question 1 ####
#To save the calculation time, I only regress the first choice on intercepts 
#and the test score. The likelihood function can be seen in mllike function.
####Question 2 ####
onerow_cho <- mlogit_data[,2:3] %>% distinct(choice_rev,.keep_all=T) %>%
  spread(choice_rev,rev_quality) 
q <- as.array(as.matrix(onerow_cho[1,],nrow=1))
datchoice <- data.frame(matrix(rep(q,nrow(mlogit_data)),byrow=TRUE,nrow =nrow(mlogit_data)))
colnames(datchoice) <- names(onerow_cho)

# change mlogit_data form (wide form)
mlogit_data <- cbind(mlogit_data[,c(1,2,3,4)],datchoice)

ni <- nrow(mlogit_data)
nj <- ncol(mlogit_data[,5:250]) #nj=246
Y <- model.matrix(~0+choice_rev,data=mlogit_data)

table(mlogit_data$choice_rev)
multi1 = multinom(choice_rev ~ score, data=mlogit_data) # reference:j=1
intial=summary(multi1)

#save the parameters from packages as initial values
write.csv(intial$coefficients,'multi1.csv')
start1=fread("./multi1.csv", header = T)
start11=mat.or.vec(2,nj-1)
start11[1,]=start1$`(Intercept)`
start11[2,]=start1$score

# matrix calculation
X=cbind(rep(1,nrow(mlogit_data)), mlogit_data[,4]) #base comparison is j=1

# In multinomial logit model x_ij = x_i. The way of one by one calculation is 
# time-consuming. This is matrix way.
mllike=function(y,x,beta) {
  beta=mat.or.vec(2,nj)
  sum_exp=as.matrix(rowSums(exp(x %*% beta[,2:nj]))) #base comparison is j=1
  mat_pro=mat.or.vec(nrow(mlogit_data),nj)
  mat_pro[,1]=1/(1+sum_exp) #base comparison is j=1
  for(i in 1:(nj-1)){
    pr=exp(x %*% beta[,i+1])/(1+sum_exp)
    pr[pr>0.999999] = 0.999999
    pr[pr<0.000001] = 0.000001
    mat_pro[,i+1]=pr
  }
  fnlike=0
  for(i in 1:nj){
    fnlike=fnlike+colSums(as.matrix(y[,i]*log(mat_pro[,i])))
  }
  return(-fnlike)
}
model1=optim(function(beta) mllike(y=Y,x=X,b=beta),par=start11,method="CG")
as.matrix(model1$par)
dim(as.matrix(model1$par))

#marginal effect
fnpro=function(x,beta){
  sum_exp=as.matrix(rowSums(exp(x %*% beta[,2:nj])))
  mat_pro=mat.or.vec(nrow(mlogit_data),nj)
  mat_pro[,1]=1/(1+sum_exp)
  for(i in 1:(nj-1)){
    pr=exp(x %*% beta[,i+1])/(1+sum_exp)
    pr[pr>0.999999] = 0.999999
    pr[pr<0.000001] = 0.000001
    mat_pro[,i+1]=pr
  }
  return(mat_pro)
}
beta1=mat.or.vec(2,nj)
beta1[,2:246]=model1$par
pij_m1=fnpro(X,beta1)
mb=c(0,model1$par[246:490])
me_model1=array(0,dim=c(nrow(X),246))
for (i in 1:nrow(X)) {
  be=sum(pij_m1[i,]*mb)
  me_model1[i,]=pij_m1[i,]*(mb-be)
}
me_model1=apply(me_model1, 2, mean)
me_model1
dim(as.matrix(me_model1))

#=================================================================
# Exercise 6: Second Model
#=================================================================
####Question 1 ####
#To save the calculation time, I only regress the first choice on intercepts 
#and the school quality. The likelihood function can be seen in mclike function.
####Question 2 ####
# matrix calculation
names(mlogit_data)[5:250] = paste('quality',names(mlogit_data)[5:250],sep='.')
mlogit_data=as.data.frame(mlogit_data)
mlogit_data_long = mlogit.data(mlogit_data,varying = 5:250,choice='choice_rev',shape='wide')
head(mlogit_data_long)
mlogit_data_long$choice=as.numeric(mlogit_data_long$choice_rev)

start2=clogit(choice ~ quality + strata(V1), data=mlogit_data_long, method = "exact")
start2$coefficients

X2 <- mlogit_data %>% 
  group_by(choice_rev) %>% 
  summarise(quality = unique(rev_quality)) %>%
  ungroup()
X2 <- as.matrix(cbind(rep(1, nj), X2[,2]))

# In conditional logit model x_ij = x_j. The way of one by one calculation is 
# time-consuming. This is matrix way.
mclike=function(y,x,beta) {
  beta=mat.or.vec(1,nj+1)
  beta[1]=0 #base comparison is j=1
  mat_pro=mat.or.vec(1,nj)
  mat_beta = mat.or.vec(2,nj)
  mat_beta[1,]= beta[1:nj]
  mat_beta[2,]= beta[nj+1]
  sum_exp=sum(diag(exp(x %*% mat_beta)))
  mat_pro=diag(exp(x %*% mat_beta))/sum_exp
  mat_pro[mat_pro>0.999999] = 0.999999
  mat_pro[mat_pro<0.000001] = 0.000001
  mat_pro <- matrix(rep(mat_pro, nrow(mlogit_data)), nrow=nrow(mlogit_data))
  fnlike=0
  for(i in 1:nj){
    fnlike=fnlike+colSums(as.matrix(y[,i]*log(mat_pro[i])))
  }
  return(-fnlike)
}
model2=optim(function(beta) mclike(y=Y,x=X2,b=beta),par=c(runif(245),start2$coefficients),method="CG")
as.matrix(model2$par)
dim(as.matrix(model2$par))

#marginal effects
fnmat_pro2=function(x,beta) {
  beta[1]=0 #base comparison is j=1
  mat_pro=mat.or.vec(1,nj)
  mat_beta = mat.or.vec(2,nj)
  mat_beta[1,]= beta[1:nj]
  mat_beta[2,]= beta[nj+1]
  sum_exp=sum(diag(exp(x %*% mat_beta)))
  mat_pro=diag(exp(x %*% mat_beta))/sum_exp
  mat_pro[mat_pro>0.999999] = 0.999999
  mat_pro[mat_pro<0.000001] = 0.000001
  return(mat_pro)
}
pj=fnmat_pro2(X2,model2$par)
mid=array(0,dim = c(nrow(X2),nj))
for (i in 1:nrow(X2)) {
  mid[i,i] <- 1
}
me_model2=array(0,dim=c(nrow(X2),nj))
for (j in 1:246) {
    for (k in 1:246) {
      me_model2[j,k]=pj[j]*(mid[j,k]-pj[k])*model2$par[247]
    }
}
colnames(me_model2)=paste('choice',1:246,sep=' ')
row.names(me_model2)=names(mlogit_data)[5:250]
dim(me_model2)

#=================================================================
# Exercise 7: Counterfactual simulations
#=================================================================
####Question 1####
#Please see the answer in the PDF.
####Question 2 ####
# The probabilities can be seen in pj in Exercise 6
####Question 3 ####
exclude_others <- mlogit_data %>%
  mutate(fil=ifelse(str_detect(choice_rev,'Others'),1,0)) %>%
  filter(fil==0) %>%
  select(1:250)
exclude_others<-exclude_others[,!str_detect(colnames(exclude_others),'Others')]
exclude_others_long = mlogit.data(exclude_others,varying = 5:200,choice='choice_rev',shape='wide')
head(exclude_others_long)
exclude_others_long$choice=as.numeric(exclude_others_long$choice_rev)

start3=clogit(choice ~ quality + strata(V1), data=exclude_others_long, method = "exact")
start3$coefficients

n2i <- nrow(exclude_others)
n2j <- ncol(exclude_others[,5:200])
Y2 <- model.matrix(~0+choice_rev,data=exclude_others)

X3 <- exclude_others %>% 
  group_by(choice_rev) %>% 
  summarise(quality = unique(rev_quality)) %>%
  ungroup()
X3 <- as.matrix(cbind(rep(1, n2j), X3[,2]))

mclike2=function(y,x,beta) {
  beta=mat.or.vec(1,n2j+1)
  beta[1]=0 #base comparison is j=1
  mat_pro=mat.or.vec(1,n2j)
  mat_beta = mat.or.vec(2,n2j)
  mat_beta[1,]= beta[1:n2j]
  mat_beta[2,]= beta[n2j+1]
  sum_exp=sum(diag(exp(x %*% mat_beta)))
  mat_pro=diag(exp(x %*% mat_beta))/sum_exp
  mat_pro[mat_pro>0.999999] = 0.999999
  mat_pro[mat_pro<0.000001] = 0.000001
  mat_pro <- matrix(rep(mat_pro, each=nrow(exclude_others)), nrow=nrow(exclude_others))
  fnlike=0
  for(i in 1:n2j){
    fnlike=fnlike+colSums(as.matrix(y[,i]*log(mat_pro[i])))
  }
  return(-fnlike)
}
model4=optim(function(beta) mclike2(y=Y2,x=X3,b=beta),par=c(runif(195),start3$coefficients),method="CG")
dim(as.matrix(model4$par))

fnmat_pro3=function(x,beta) {
  beta[1]=0 #base comparison is j=1
  mat_pro=mat.or.vec(1,n2j)
  mat_beta = mat.or.vec(2,n2j)
  mat_beta[1,]= beta[1:n2j]
  mat_beta[2,]= beta[n2j+1]
  sum_exp=sum(diag(exp(x %*% mat_beta)))
  mat_pro=diag(exp(x %*% mat_beta))/sum_exp
  mat_pro[mat_pro>0.999999] = 0.999999
  mat_pro[mat_pro<0.000001] = 0.000001
  return(mat_pro)
}
pj2=fnmat_pro3(X3,model4$par)
dim(as.matrix(pj2))
dim(as.matrix(pj))