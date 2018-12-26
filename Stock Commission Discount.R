
setwd("E:\\Data Science Study\\Data Sets\\R programming")
## ----
rg_train=read.csv("rg_train.csv",stringsAsFactors = FALSE)
rg_test=read.csv("rg_test.csv",stringsAsFactors = FALSE)

library(dplyr)
glimpse(rg_train)

#Function to create dummie variables
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

rg_test$Revenue.Grid=NA

rg_train$data='train'
rg_test$data='test'

#Combine train and test data for Data Wrangling
rg=rbind(rg_train,rg_test)

table(rg$children)

rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
         children=as.numeric(children))

table(rg$age_band)

## ----
rg=rg %>%
mutate(a1=as.numeric(substr(age_band,1,2)),
       a2=as.numeric(substr(age_band,4,5)),
      age=ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
       ) %>%
  select(-a1,-a2,-age_band)

## we could have done something similar for variable family income also

## ------------------------------------------------------------------------
glimpse(rg)


lapply(rg,function(x) length(unique(x)))

#Check Categorical Variables
names(rg)[sapply(rg,function(x) is.character(x))]

cat_cols=c("status","occupation","occupation_partner","home_status","family_income","self_employed",
           "self_employed_partner","TVarea","gender","region")

#Create dummy variables for categorical variables
for(cat in cat_cols){
  rg=CreateDummies(rg,cat,50)
}

glimpse(rg)

#Drop Post_Code and Post_area since these columns doesn't bring much information
rg=rg %>%
  select(-post_code,-post_area)

sum(sapply(rg,function(x) is.character(x)))

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)

#Check if any observation has NAs
lapply(rg,function(x) sum(is.na(x)))

#Replace NAs with mean except for data and target variable
for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

## Test Train Split
rg_train=rg %>% filter(data=='train') %>% select(-data)
rg_test=rg %>% filter(data=='test') %>% select (-data,-Revenue.Grid)

#Break training data for training the model and its validation
set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]

##  Model Building
library(car)

for_vif=lm(Revenue.Grid~.-REF_NO,data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]

## from here we'll remove vars with high vif one by one, code below is arrived
## at after multiple iterations

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
           -Investment.in.Derivative-Investment.in.Equity
           -region_SouthEast-TVarea_Central-occupation_Professional
           -family_income_GT_EQ_35000-region_Scotland
           -Portfolio.Balance,
           data=rg_train1)

sort(vif(for_vif),decreasing = T)[1:3]
###

log_fit=glm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
            -Investment.in.Derivative-Investment.in.Equity
            -region_SouthEast-TVarea_Central-occupation_Professional
            -family_income_GT_EQ_35000-region_Scotland
            -Portfolio.Balance,data=rg_train1,family = "binomial")

log_fit=step(log_fit)

formula(log_fit)

log_fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
              Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
              Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
              Home.Loan + Online.Purchase.Amount + occupation_Unknown + 
              home_status_RentPrivately + family_income_LT_25000GT_EQ_22500 + 
              family_income_LT_30000GT_EQ_27500 + family_income_LT_27500GT_EQ_25000 + 
              self_employed_partner_No + TVarea_ScottishTV + TVarea_Meridian + 
              region_Unknown,data=rg_train,family='binomial')
summary(log_fit)

# from here we can drop vars one by one which had higher p-value
# code given below is result of multiple iterations
log_fit=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
              Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
              Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
              Home.Loan + Online.Purchase.Amount  + 
              family_income_LT_30000GT_EQ_27500  + 
              self_employed_partner_No + TVarea_ScottishTV + TVarea_Meridian ,
            data=rg_train,family='binomial')
summary(log_fit)

#### performance of score model on validation data
library(pROC)

val.score=predict(log_fit,newdata = rg_train2,type='response')

auc(roc(rg_train2$Revenue.Grid,val.score))

# so the tentative score performance of logistic regression is going to be around 0.95
# now lets build the model on entire training data

# code given below is result of multiple iterations
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
           -Investment.in.Derivative-Investment.in.Equity
           -region_SouthEast-TVarea_Central-occupation_Professional
           -family_income_GT_EQ_35000-region_Scotland-Portfolio.Balance
           ,data=rg_train)

sort(vif(for_vif),decreasing = T)[1:3]

log.fit.final=glm(Revenue.Grid~.-REF_NO-Investment.in.Commudity
                  -Investment.in.Derivative-Investment.in.Equity
                  -region_SouthEast-TVarea_Central-occupation_Professional
                  -family_income_GT_EQ_35000-region_Scotland-Portfolio.Balance,
                  data=rg_train,family='binomial')

log.fit.final=step(log.fit.final)

summary(log.fit.final)

formula(log.fit.final)

log.fit.final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
                    Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                    Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
                    Home.Loan + Online.Purchase.Amount + status_Partner + occupation_partner_Retired + 
                    self_employed_partner_No + TVarea_ScottishTV + TVarea_Meridian + 
                    gender_Female,
                  data=rg_train,family='binomial')

summary(log.fit.final)

test.prob.score= predict(log_fit,newdata = rg_test,type='response')

# however if we need hard classes, we'll need to determine cutoff score
## KS Score 

train.score=predict(log.fit.final,newdata = rg_train,type='response')
real=rg_train$Revenue.Grid
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)

    cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS))
}

cutoff_data=cutoff_data[-1,]


#### visualise how this measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:KS)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()

## Highest KS Score as cutoff
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# now that we have our cutoff we can convert score to hard classes
test.predicted=as.numeric(test.prob.score>my_cutoff)