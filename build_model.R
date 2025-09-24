library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(xgboost)
library(SHAPforxgboost)
library(zoo)
library(caret)



#load model data

data_files <- list.files("model_data/",full.names = T)

full_data <- data.frame()

for(file in data_files) {
  
year_data <- readRDS(file)

full_data <- rbind(full_data,year_data )
  
}
#Select metric to predict

metric = "home_beat_spread"

#only use up too the last year 

model_data <- full_data |>    filter( season < max(season)) |> select(-c(game_result,season,game_id,home_team,week,away_team)) 

val_data <- full_data |>  filter( season == max(season)) |> select(-c(game_result,season,game_id,home_team,week,away_team)) 




#built train test sets

set.seed(123)

train_indices <- createDataPartition(model_data$home_beat_spread, p = 0.7, list = FALSE)
train_df <- model_data[train_indices, ]
test_df <- model_data[-train_indices, ]

x_train <-   train_df %>% select(-c(("home_beat_spread")))
y_train <-   train_df %>%  pull ("home_beat_spread")

x_test <-   test_df %>% select(-c(("home_beat_spread")))
y_test <-   test_df %>% pull ("home_beat_spread")



#create test and training sets and dependent variable vectors
xgb_train<-data.matrix(x_train)
xgb_test<-data.matrix(x_test)
xgb_train_dv<-y_train
xgb_test_dv<-y_test

#create DMatrix objects required for XGBoost
xgb_train_DMatrix<-xgb.DMatrix(data = xgb_train, label = xgb_train_dv)
xgb_test_DMatrix<-xgb.DMatrix(data = as.matrix(xgb_test), label = xgb_test_dv)

params<-list(booster = "gbtree", objective = "binary:logistic", eta=0.1, 
             gamma=0, max_depth=3, min_child_weight=1, 
             subsample=.8, colsample_bytree=.8)

#perform cross validation to determine the optimal number of trees to train
cv3<-xgb.cv(params = params, data = xgb_train_DMatrix, nrounds = 200, nfold = 5, showsd = T, stratified = T,
            print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb3<- xgb.train(params = params, data = xgb_train_DMatrix, nrounds = cv3$best_iteration, 
                 watchlist = list(test=xgb_test_DMatrix,train=xgb_train_DMatrix), print_every_n = 5, 
                 early_stopping_rounds = 20, maximize = F)
#model prediction
xgbpred3 <- predict (xgb3, xgb_test_DMatrix)





# Predict outcome on test data
xgbpred_prob3 <- predict (xgb3, xgb_test_DMatrix, type = 'prob')
y_pred_num3 <- as.factor(ifelse(xgbpred_prob3 > 0.5, "YES", "NO"))

xgb_test_dv_fac3<- as.factor(ifelse(xgb_test_dv == 1, "YES", "NO"))

# Print Confusion matrix, & F1 score
cm3 <-confusionMatrix(as.factor(y_pred_num3), as.factor(xgb_test_dv_fac3), positive = "YES")
cm3

## SHAP analysis

shap_values3<-shap.values(xgb_model = xgb3, X_train = xgb_train)

shap_long3<-shap.prep(xgb_model = xgb3, X_train = as.matrix(xgb_train))
shap.plot.summary(shap_long3)

thresh<-data.frame(Actual = y_test, Prob = xgbpred3)


for (t in c(.75, .66, .6, .55)) {
  
  new<-ifelse((thresh$Actual == 1 & thresh$Prob >= t), 1,
              ifelse(thresh$Actual == 0 & thresh$Prob <=1-t, 1,
                     ifelse(thresh$Actual == 1 & thresh$Prob < 1-t, 0,
                            ifelse(thresh$Actual == 0 & thresh$Prob > t,0, NA))))
  
  thresh[, ncol(thresh) + 1] <- new
  
  colnames(thresh)[ncol(thresh)] <- paste0(t, " thresh")
  
}

thresh$Actualf <- as.factor(thresh$Actual)

ggplot(thresh, aes(x = Prob, group = Actualf,colour = Actualf)) +geom_histogram(alpha = 0.1, position = "identity")





val_df <- val_data

x_val <-   val_df %>% select(-c(("home_beat_spread")))
y_val <-  val_df %>% pull ("home_beat_spread")


xgb_val<-data.matrix(x_val)

xgb_val_dv<-y_val


xgb_val_DMatrix<-xgb.DMatrix(data = as.matrix(xgb_val), label = xgb_val_dv)

#model prediction
xgbpred_val <- predict (xgb3, xgb_val_DMatrix)



y_pred_num3 <- as.factor(ifelse(xgbpred_val > 0.5, "YES", "NO"))

xgb_test_dv_fac3<- as.factor(ifelse(xgb_val_dv == 1, "YES", "NO"))

# Print Confusion matrix, & F1 score
cmval <-confusionMatrix(as.factor(y_pred_num3), as.factor(xgb_test_dv_fac3), positive = "YES")
cmval



thresh<-data.frame(Actual = y_val, Prob = xgbpred_val)


for (t in c(.75, .66, .6, .55)) {
  
  new<-ifelse((thresh$Actual == 1 & thresh$Prob >= t), 1,
              ifelse(thresh$Actual == 0 & thresh$Prob <=1-t, 1,
                     ifelse(thresh$Actual == 1 & thresh$Prob < 1-t, 0,
                            ifelse(thresh$Actual == 0 & thresh$Prob > t,0, NA))))
  
  thresh[, ncol(thresh) + 1] <- new
  
  colnames(thresh)[ncol(thresh)] <- paste0(t, " thresh")
  
}

thresh$Actualf <- as.factor(thresh$Actual)

ggplot(thresh, aes(x = Prob, group = Actualf,colour = Actualf)) +geom_histogram(alpha = 0.1, position = "identity")


