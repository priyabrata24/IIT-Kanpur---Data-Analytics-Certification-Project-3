setwd("D:/My Docs/IIT Kanpur - Data Analytics/Projects/P Project Problem -3 Manufacturing")
getwd
product_test=read.csv("product_test.csv",stringsAsFactors = F)
product_train=read.csv("product_train.csv", stringsAsFactors = F)

library(dplyr)
library(car)

product_test$went_on_backorder=NA

product_test$data="test"
product_train$data="train"

product_all=rbind(product_train,product_test)

lapply(product_all,function(x) sum(is.na(x)))

product_all=product_all %>% 
  select(-sku)

product_all=product_all %>% 
  mutate(potential_issue=as.numeric(potential_issue=="Yes"))

product_all=product_all %>% 
  mutate(deck_risk=as.numeric(deck_risk=="Yes"))

product_all=product_all %>% 
  mutate(oe_constraint=as.numeric(oe_constraint=="Yes"))

product_all=product_all %>% 
  mutate(ppap_risk=as.numeric(ppap_risk=="Yes"))

product_all=product_all %>% 
  mutate(stop_auto_buy=as.numeric(stop_auto_buy=="Yes"))

product_all=product_all %>% 
  mutate(rev_stop=as.numeric(rev_stop=="Yes"))

# CreateDummies=function(data,var,freq_cutoff=0){
#   t=table(data[,var])
#   t=t[t>freq_cutoff]
#   t=sort(t)
#   categories=names(t)[-1]
#   
#   for( cat in categories){
#     name=paste(var,cat,sep="_")
#     name=gsub(" ","",name)
#     name=gsub("-","_",name)
#     name=gsub("\\?","Q",name)
#     name=gsub("<","LT_",name)
#     name=gsub("\\+","",name)
#     
#     data[,name]=as.numeric(data[,var]==cat)
#   }
#   
#   data[,var]=NULL
#   return(data)
# }

#product_all=CreateDummies(product_all ,"potential_issue",100)

product_train=product_all %>% filter(data=='train') %>% select(-data)
product_test=product_all %>% filter(data=='test') %>% select(-data,-went_on_backorder)

product_train=product_train %>% 
  mutate(went_on_backorder=as.numeric(went_on_backorder=="Yes"))

set.seed(2)
s=sample(1:nrow(product_train),.75*nrow(product_train))
product_train1=product_train[s,]
product_train2=product_train[-s,]

library(pROC)

#Logistic Regression Model
for_vif=lm(went_on_backorder~.,data=product_train1)

sort(vif(for_vif),decreasing = T)

for_vif=lm(went_on_backorder~.-forecast_6_month-sales_6_month-forecast_9_month
           -sales_9_month-sales_1_month-perf_12_month_avg-min_bank
           ,data=product_train1)

log_fit=glm(went_on_backorder~.-forecast_6_month-sales_6_month-forecast_9_month
            -sales_9_month-sales_1_month-perf_12_month_avg-min_bank
            ,data=product_train1)

log_fit
log_fit=step(log_fit)

summary(log_fit)

formula(log_fit)

val.score=predict(log_fit,newdata = product_train2,type='response')

auc(roc(product_train2$went_on_backorder,val.score))

#DTree Model
library(rpart)
library(rpart.plot)
library(tidyr)
require(rpart)
library(tree)

dtModel = tree(went_on_backorder~.,data=product_train1)
plot(dtModel)
dtModel

val.score=predict(dtModel,newdata = product_train2)
auc(roc(product_train2$went_on_backorder,val.score))

#Random Forest Model
library(randomForest)
randomForestModel = randomForest(went_on_backorder~.,data=product_train1)
d=importance(randomForestModel)
d
names(d)
d=as.data.frame(d)
d$IncNodePurity=rownames(d)
d %>% arrange(desc(IncNodePurity))

val.score=predict(randomForestModel,newdata = store_train2)
auc(roc(store_train2$store,val.score))

#GBM Model
library(gbm)
library(cvTools)

param=list(interaction.depth=c(1:10),
           n.trees=c(700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  set.seed(2)
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=5
my_params=subset_paras(param,num_trials)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

myauc=0

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  
  params=my_params[i,]
  
  k=cvTuning(gbm,went_on_backorder~.,data=product_train,
             tuning =params,
             args = list(distribution="bernoulli"),
             folds = cvFolds(nrow(product_train), K=10, type = "random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response",n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    
    myauc=score.this
    print(myauc)
    
    best_params=params
  }
  print('DONE')
}

myauc

best_params

best_params=data.frame(interaction.depth=2,
                       n.trees=700,
                       shrinkage=0.01,
                       n.minobsnode=5)

product.gbm.final=gbm(went_on_backorder~.,data=product_train,
                      n.trees = best_params$n.trees,
                      n.minobsinnode = best_params$n.minobsnode,
                      shrinkage = best_params$shrinkage,
                      interaction.depth = best_params$interaction.depth,
                      distribution = "bernoulli")

product.gbm.final

test.pred=predict(product.gbm.final,newdata=product_test,
                  n.trees = best_params$n.trees,type="response")

#Cutoff
train.score=predict(product.gbm.final,newdata = product_train,
                    n.trees = best_params$n.trees,type='response')

real=product_train$went_on_backorder
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

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
  
  #print(paste0('KS Score: ',KS))
  
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

max(cutoff_data$KS)
1-(0.025/0.6886201)

View(cutoff_data)

#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=M))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# now that we have our cutoff we can convert score to hard classes

test.predicted=(test.pred>my_cutoff)
test.predicted
write.table(test.predicted,file ="Abhilash_Singh_P3_part2.csv",
            row.names = F,col.names="went_on_backorder")


############################Quiz############################################
#1
s1=product_train %>% 
  filter(went_on_backorder=="Yes") %>% 
  select(perf_6_month_avg,perf_12_month_avg)

median(s1$perf_6_month_avg)
median(s1$perf_12_month_avg)

View(product_train)

#2
chisq.test(product_train$deck_risk,product_train$went_on_backorder)

#3
dim(product_train)

s2=product_train %>% 
  select(pieces_past_due) %>% 
  filter(pieces_past_due==0)
dim(s2)
round(247909*100/250078,2)

#4

#5
sqrt(30)

#6

#7
round(cor(product_train$forecast_9_month, product_train$sales_9_month),3)
cor.test(product_train$forecast_9_month, product_train$sales_9_month)

#8
t.test(product_train$min_bank~product_train$went_on_backorder)

store_train %>% 
  mutate(total_sales=sales0+sales1+sales2+sales3+sales4) %>% 
  select(total_sales,store_Type) %>% 
  group_by(store_Type) %>% 
  summarise(var=var(total_sales)) %>% 
  filter(var==max(var))

#9
glimpse(store_train)
n1=table(store_train$state_alpha)
dim(n1)
sort(n1)
View(n1)

n2=store_train %>% 
  mutate(total_sales=sales0+sales1+sales2+sales3+sales4) %>% 
  group_by(state_alpha) %>% 
  summarise(avg=mean(total_sales)) %>% 
  select(state_alpha,avg)
View(n2)
min(n2$avg)
max(n2$avg)

n3=cbind(n1,n2)
View(n3)
sum(n3$Freq)

#10
class(store_train$store)