getwd()
library(dplyr)
library(ggplot2)
library(mice)

h_train = read.csv('E:/Edvancer/R programming/Real Estate/housing_train.csv')
h_test = read.csv('E:/Edvancer/R programming/Real Estate/housing_test.csv')

dim(h_train)
dim(h_test)

h_train$data = 'Train'
h_test$data = 'Test'

glimpse(h_train)
glimpse(h_test)

h_test$Price = NA

h_all = rbind(h_train,h_test)

summary(h_all)

unlist(lapply(h_all, function(x) sum(is.na(x))))

impute = h_all[c('Bedroom2','Bathroom','Car','Landsize','BuildingArea','YearBuilt')]

set.seed(2)
imputed = complete(mice(impute))

h_all$Bedroom2 = imputed$Bedroom2
h_all$Bathroom = imputed$Bathroom
h_all$Car = imputed$Car
h_all$Landsize = imputed$Landsize
h_all$BuildingArea = imputed$BuildingArea
h_all$YearBuilt = imputed$YearBuilt

glimpse(h_all)



#Staring data preparation
#Creating Dummy variable

#creating dummy for Rooms variable
# h_all = h_all %>% mutate(rooms_1 = as.numeric(Rooms==1),
#                          rooms_2 = as.numeric(Rooms==2),
#                          rooms_3 = as.numeric(Rooms==3),
#                          rooms_4 = as.numeric(Rooms==4),
#                          rooms_5 = as.numeric(Rooms==5)) %>%
#                   select(-Rooms)

#creatiing dummy for Type variable
h_all = h_all %>% mutate(type_h = as.numeric(Type == 'h'),
                         type_t = as.numeric(Type == 't'),
                         type_u = as.numeric(Type == 'u')) %>%
                  select(-Type)


#creating dummy for method variable
h_all = h_all %>% mutate(Mtd_PI = as.numeric(Method == 'PI'),
                         Mtd_S = as.numeric(Method == 'S'),
                         Mtd_SA = as.numeric(Method == 'SA'),
                         Mtd_SP = as.numeric(Method == 'SP'),
                         Mtd_VB = as.numeric(Method == 'VB'))%>%
                  select(-Method)

#creating dummy for car variable
# h_all = h_all %>% mutate(car_0 = as.numeric(Car == 0),
#                  car_1 = as.numeric(Car == 1),
#                  car_2 = as.numeric(Car == 2),
#                  car_3 = as.numeric(Car == 3),
#                  car_4 = as.numeric(Car == 4)) %>%
#           select(-Car)

#dropping SellerG varibale

h_all$SellerG = NULL


# creating dummy for Council

sort(table(h_all$CouncilArea)/nrow(h_all)*100, decreasing = TRUE)

h_all %>%
  group_by(CouncilArea) %>%
  summarise(h = mean(Price,na.rm = T)) %>%
  arrange(h)

# maribyrnong, moreland = 7
# darebin = 8
# banyule, melbourne, kington, moonee valley, hobsons bay = 9
# monash, blank, glen eiran port phillip = 10
# Yarra, Manninggham = 11
# whitehorse,stonnington =12
# boroondara, bayside = 16

h_all = h_all %>% 
  mutate(ca_7 = as.numeric(CouncilArea == c('Maribyrnong','Moreland')),
         ca_8 = as.numeric(CouncilArea == 'Darebin'),
         ca_9 = as.numeric(CouncilArea== c("Banyule","Melbourne","Kingston","Moonee Valley","Hobsons Bay")),
         ca_10 = as.numeric(CouncilArea==c("Monash","","Glen Eira","Port Phillip")),
         ca_11 = as.numeric(CouncilArea==c("Yarra","Manningham")),
         ca_13 = as.numeric(CouncilArea==c("Whitehorse","Stonnington")),
         ca_16 = as.numeric(CouncilArea== 'Bayside')) %>%
  select(-CouncilArea)

# creating dummy for bedrrom variable

# h_all = h_all %>% mutate(bd_1 = as.numeric(Bedroom2 ==1),
#                  bd_2 = as.numeric(Bedroom2 ==2),
#                  bd_3 = as.numeric(Bedroom2 ==3),
#                  bd_4 = as.numeric(Bedroom2 ==4),
#                  bd_5 = as.numeric(Bedroom2 ==5)) %>%
#   select(-Bedroom2)

# dummy for bathroom

# h_all = h_all %>% mutate(br_1 = as.numeric(Bathroom == 1),
#                  br_2= as.numeric(Bathroom == 2),
#                  br_3= as.numeric(Bathroom == 3)) %>%
#   select(-Bathroom)


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) ## calculating the frequency of a column
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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

h_all = CreateDummies(h_all,'Suburb',500)
# h_all = CreateDummies(h_all,'Postcode',100)
# h_all = CreateDummies(h_all,'YearBuilt',100)

h_all$Address = NULL

# unlist(lapply(h_all, function(x) sum(is.na(x))))
# 
# replace_na = function(x) {
#   x[is.na(x)] = mean(x,na.rm = TRUE)
#   return (x)
# }
# 
# #columns names which has missing and non-missing values
# missing_columns = unlist(lapply(h_all, function(x) sum(is.na(x))))
# 
# names = missing_columns[missing_columns>0]
# ## pick only columns where finally there is atleast one value which is missing
# 
# final_columns_to_be_imputed = setdiff(names(missing_columns[missing_columns>0]),'Price')
# 
# ## replacing all the missing instances with column means using predefined function
# 
# h_all[,final_columns_to_be_imputed] = lapply(h_all[,final_columns_to_be_imputed], replace_na)
# 
# ## checking again if there are any missing values

unlist(lapply(h_all, function(x) sum(is.na(x))))

h_train = h_all %>% filter(data == 'Train') %>% select(-data)
h_test  = h_all %>% filter(data == 'Test') %>% select(-data)

set.seed(7)
s = sample(1:nrow(h_train),0.7*nrow(h_train))
h_train1 = h_train[s,]
h_train2 = h_train[-s,]

dim(h_train1)
dim(h_train2)


# Building randomforest model

library(randomForest)
library(caret)
library(cvTools)

rf = randomForest(Price~.,data = h_train1)

rf.predict = predict(rf,h_train2)

error = (h_train2$Price - rf.predict)**2

error %>% mean() %>% sqrt()

#RMSE 324944

# hypertunning

param = list(mtry = c(5,10,15,20,25),
              ntree = c(50,100,150,200,250),
              maxnodes = c(5,10,20,30,40,50),
              nodesizes = c(1,2,5,10))

expand.grid(param)

# function for creating random subset from param

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)

myerror=9999999


for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =h_train1,
             tuning =params,
             folds = cvFolds(nrow(h_train1), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    # print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    # print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

rf.final = randomForest(Price~.,
                        mtry = best_params$mtry,
                        ntree=best_params$ntree,
                    maxnodes=best_params$maxnodes,nodesizes = best_params$nodesizes,data = h_train1)

rf.predict.h_train1 = predict(rf.final,newdata = h_train2)

error = h_train2$Price - rf.predict.h_train1

error**2 %>% mean() %>% sqrt()

#RMSE 362581.5

rf.final.htrain = randomForest(Price~.,
                        mtry = best_params$mtry,
                        ntree=best_params$ntree,
                        maxnodes=best_params$maxnodes,nodesizes = best_params$nodesizes,data = h_train)

rf.predict.test = predict(rf.final.htrain,newdata = h_test)


write.csv(rf.predict.test,'RandomForest.csv')

# linear regression model

library(car)


fit = lm(Price ~., data = h_train1)

alias(lm(Price~.,data = h_train1))

fit = lm(Price ~.-type_u-Mtd_VB , data = h_train1)

sort(vif(fit), decreasing = TRUE)


fit = step(fit)

form = formula(fit)

form = Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
  Landsize + BuildingArea + YearBuilt + type_h + type_t  + 
  Mtd_S + ca_7 + ca_8  + ca_11 + ca_13 + ca_16

fit = lm(form, data = h_train1)

summary(fit)

val.pred = predict(fit,newdata = h_train2)
error = h_train2$Price - val.pred

error**2 %>% mean() %>% sqrt()


fit.final = lm(Price~. , data = h_train)

fit.final = step(fit.final)

test.pred = predict(fit.final, newdata = h_test)

write.csv(test.pred,'submission1.csv',row.names = F)



plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

#### 

output=summary(fit.final)
names(output)

output$coefficients[,4]
