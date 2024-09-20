rm(list = ls()) 
# library ----
library(caret) 
library(OptimalCutpoints)
library(rms)
library(Matrix)
library(openxlsx)
library(tidyverse)
library(rmda)


exp <- read.csv("D:/Mine-yan/alpha_cfCRC.csv",header=TRUE,encoding = 'UTF-8', row.names = 1)

cl <- read.xlsx("D:/Mine-yan/cl_baseline.xlsx", 1, colNames = T, rowNames = T)
dim(cl)

set.seed(127)
inA <- createDataPartition(cl$obs, p = 0.7,list = F)
row.names(inA) <- row.names(cl)[inA]
cl$group <- ifelse(row.names(cl) %in% row.names(inA), "training", "validation")
train_exp <- exp[inA,]
train_cl <- cl[inA,]
test_exp <- exp[-inA,]
test_cl <- cl[-inA,]

# section 1 LASSO----
y <- as.factor(train_cl[,"obs"])
x <- as.matrix(train_exp[])
y <- as.factor(train_cl_1[,"obs"])
x <- as.matrix(train_exp_1[])

# y <- scale(y)   
## LASSO ----
set.seed(127)
alpha1_fit <- glmnet(x,y,alpha=1,family="binomial",nlambda = 100, standardize = F)
plot(alpha1_fit,xvar="lambda",label=F)
alpha1.fit <- cv.glmnet(x,y,type.measure = "class",alpha=1,family="binomial",standardize = F)
best_lambda <- alpha1.fit$lambda.min   #最佳Lambda
best_fit <- glmnet(x, y, alpha = 1, lambda = best_lambda,family="binomial",standardize = F)
Coefficients<- coef(best_fit, s=best_lambda)
select_feature <- rownames(best_fit$beta)[as.numeric(best_fit$beta)!=0]#as.numeric后"."会转化为0
length(select_feature)
summary(alpha1.fit)
log(alpha1.fit$lambda.min)  
log(alpha1.fit$lambda.1se)  
plot(alpha1.fit)
print(alpha1.fit)
print(best_fit)

## section 2 REF ----
y <- factor(y, levels = c(0,1), labels = c('Control','Case'))
x <- as.data.frame(x)
rfeControl1 = rfeControl(functions = caretFuncs,#svm
                        method = "cv", 
                        saveDetails = T, 
                        number = 5,
                        allowParallel = T) 
svmProfile <- rfe(x, y,
                  sizes = c(2:200),
                  rfeControl = rfeControl1,
                  method = "svmRadial")
which.max(svmProfile$results$Accuracy)

ggplot(data = svmProfile, metric = "Accuracy") + 
  scale_x_continuous(limits = c(0, 200)) +
  geom_line(size = 0.7,color = 'pink') + 
  geom_point(color= 'orange')+
  theme_minimal()+
  theme_bw()

# section 3 Intersect (feature selection) ----
Intersect <- intersect(select_feature,predictors(svmProfile)) 

