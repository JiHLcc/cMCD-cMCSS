#  XGBoost ----
library(xgboost)
library(dcurves)
library(tidymodels)
library(themis)
library(scales)

training_dataset <- cbind(train_cl[,c('obs')],train_exp[,Intersect])
colnames(training_dataset)[1] <- c('obs')
validation_dataset <- cbind(test_cl[,c('obs')],test_exp[,Intersect])
colnames(validation_dataset)[1] <- c('obs')

X_train <- as.matrix(training_dataset[, -1])
X_train <- Matrix(X_train,sparse = T)
y_train <- as.character(training_dataset$obs)
traindata <- list(data=X_train,label=y_train) 

X_test <- as.matrix(validation_dataset[, -1])
X_test <- Matrix(X_test,sparse=T)
y_test <- as.character(validation_dataset$obs)
testset <- list(data=X_test,label=y_test)

dtrain <- xgb.DMatrix(data = traindata$data, label = traindata$label)
dtest <- xgb.DMatrix(data = testset$data, label = testset$label)
watchlist <- list(train = dtrain, eval = dtest)
params <- list(objective = "binary:logistic", eval_metric = "auc",  
               eta = 1,verbose=-1, max_depth = 3, lambda = 1, alpha = 1)
# model training
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 200, watchlist = watchlist)
str(xgb_model)

train_predictions <- predict(xgb_model, newdata = dtrain)
modelxg1_1 <- roc(y_train,train_predictions,ci = TRUE)
coords(modelxg1_1, "best")

test_predictions <- predict(xgb_model, newdata = dtest)
modelxg2_1 <- roc(y_test,test_predictions,ci = TRUE)
coords(modelxg2_1, "best")

## xgb.cv
param_grid <- list(
  max_depth = c(3,6,8,10),
  eta = c(0.01, 0.1,0.2),
  subsample = c(0.6,0.8),
  colsample_bytree = c(0.7),
  gamma = c(0.1,0.5,0.3),
  min_child_weight = c(6,8),
  lambda = c(0.5,1,5,8,10),
  alpha = c(0.5,1,5,8,10))
best_params <- list()
#min_logloss <- Inf
max_auc <- -Inf
best_iter <- integer()
for (max_depth in param_grid$max_depth) {
  for (eta in param_grid$eta) {
    for (subsample in param_grid$subsample) {
      for (colsample_bytree in param_grid$colsample_bytree) {
        for(gamma in param_grid$gamma){
          for(min_child_weight in param_grid$min_child_weight){
            for(lambda in param_grid$lambda){
              for(alpha in param_grid$alpha){
                params <- list(
                  objective = "binary:logistic",
                  eval_metric = "auc",
                  #num_class = 3,
                  max_depth = max_depth,
                  eta = eta,
                  subsample = subsample,
                  colsample_bytree = colsample_bytree,
                  gamma = gamma,
                  min_child_weight = min_child_weight,
                  lambda = lambda,
                  alpha = alpha
                )
                cv_model <- xgb.cv(
                  params = params,
                  data = dtrain,
                  nrounds = 100,
                  nfold = 10,
                  early_stopping_rounds = 20,
                  print_every_n = 100,
                  maximize = FALSE,
                  watchlist = watchlist
                )
                mean_auc <- mean(cv_model$evaluation_log$test_auc_mean)
                if (mean_auc > max_auc) {
                  max_auc <- mean_auc
                  best_params <- params
                  best_iter <- which.max(cv_model$evaluation_log$test_auc_mean)
                }   
              }
            }
          }
        }
      }
    }
  }
}
print(best_params)

params <- list(objective = "binary:logistic", eval_metric = "auc",# num_class = 3, 
               eta = best_params$eta, max_depth = 10, 
               gamma = best_params$gamma,
               colsample_bytree = best_params$colsample_bytree,
               min_child_weight = best_params$min_child_weight,
               subsample = best_params$subsample, 
               lambda = best_params$lambda,
               alpha = best_params$alpha)

## Final model ---
set.seed(1) 
xgb_model_final <- xgb.train(params = params, data = dtrain, nrounds = 100, watchlist = watchlist,
                             early_stopping_rounds = 20, print_every_n = 100)
### training dataset
train_predictions <- predict(xgb_model_final, newdata = dtrain)
modelxg1 <- roc(y_train,train_predictions,ci = TRUE)
auc(modelxg1)
ci(modelxg1)
coords(modelxg1, "best")
train_cl$xgb <- train_predictions

### validation dataset
test_predictions <- predict(xgb_model_final, newdata = dtest)
modelxg2 <- roc(y_test,test_predictions,ci = TRUE)
auc(modelxg2)
ci(modelxg2)
coords(modelxg2, "best")
test_cl$xgb <- test_predictions

### combined dataset
cl_1 <- rbind(train_cl, test_cl)
cl_1$xgb1 <- rescale(cl_1$xgb)
oc <- optimal.cutpoints(X="xgb",
                        status = "obs",
                        tag.healthy="0",
                        method="Youden",#支持的标准超多,MaxSp“（最大化特异性）“最大灵敏度”MaxSpSe“（同时最大化敏感性和特异性）
                        data=cl_1)
summary(oc)
roc_all_xgb <- roc(cl_1$obs,cl_1$xgb,ci = TRUE)

##calibration and DCA ---
p <- sort(cl_1$xgb) 
p <- as.numeric(p)
predy <- seq(p[5], p[nrow(cl_1) - 4], length = 50)
smo <- lowess(cl_1$xgb, as.numeric(cl_1$obs), iter = 0)
plot(smo)
plotdat<-as.data.frame(smo)
ggplot(plotdat, aes(x, y))+ 
  geom_line(aes(x = x, y = y), linewidth=3,col="tomato", lty =2)+
  annotate(geom = "segment", x = 0, y = 0, xend =1, yend = 1,color = "black",size=1.5,lty =5)+ 
  xlab("Predicted Probability") + ylab("Observed Probability") +
  theme_minimal()+
  theme_bw()

measure_data <- cl_1 %>%           
  select(obs) %>%           
  bind_cols(xgb=train_predictions)
dca(data=measure_data,obs~xgb,
    label = list(xgb = "Prediction Model")) %>%
  plot(smooth = TRUE) +
  ggplot2::labs(x = "Treatment Threshold Probability")
