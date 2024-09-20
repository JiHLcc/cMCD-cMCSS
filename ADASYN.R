###处理不平衡数据
library(UBL)
library(dplyr)
library(caret) 
library(OptimalCutpoints)
library(rms)
library(Matrix)
library(openxlsx)

# extract characteristics to construct models for stage stratification
Var <- c('stage') #for cMCSS
Var_1 <- c('stage','CEA','CA199','CA724') #for tumormarkers

train_cl_1 <- train_cl[train_cl$stage == 1 ,Var]  # Var/Var_1: for different models
train_exp_1 <- train_exp[rownames(train_cl_1),Intersect]
colnames(train_cl_1)[1] <- c('obs')
test_cl_1 <- test_cl[test_cl$stage == 1 ,Var] # Var/Var_1: for different models
colnames(test_cl_1)[1] <- c('obs')
test_exp_1 <- test_exp[rownames(test_cl_1),Intersect]


train_data_1 <- cbind(train_cl_1[1], train_exp_1 )
colnames(train_data_1)

train_data_1$obs<- as.factor(train_data_1$obs)

##for construction model of tumormarkers
#train_data_1$CEA<- as.factor(train_data_1$CEA)
#train_data_1$CA199<- as.factor(train_data_1$CA199)
#train_data_1$CA724<- as.factor(train_data_1$CA724)

##ADASYN
set.seed(100)
newData<- AdasynClassif(obs~ ., train_data_1, beta = 1)
table(train_data_1$obs)
table(newData$obs)

write.csv(newData, file = 'cl_For_cMCSS.csv')

training_dataset <- newData
validation_dataset <- cbind(test_cl_1[,1],test_exp_1)
