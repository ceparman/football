library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(xgboost)
library(SHAPforxgboost)
library(zoo)
library(caret)

model <- readRDS("9_24_25_model.rds")

#load model data

data_files <- list.files("current_year/",full.names = T)

full_data <- data.frame()

for(file in data_files) {
  
year_data <- readRDS(file)

full_data <- rbind(full_data,year_data )
  
}
#Select metric to predict

metric = "home_beat_spread"

#only use up too the last year 

#model_data <- full_data |>    filter( season < max(season)) |> select(-c(game_result,season,game_id,home_team,week,away_team)) 

current_week <- 4

current_data <- full_data  |> filter(week == current_week) |> select(-c(game_result,season,game_id,home_team,week,away_team))




set.seed(123)



x_test <-   current_data %>% select(-c(("home_beat_spread")))
y_test <-   current_data %>% pull ("home_beat_spread")



#create test and training sets and dependent variable vectors

xgb_test<-data.matrix(x_test)

xgb_test_dv<-y_test

#create DMatrix objects required for XGBoost

xgb_test_DMatrix<-xgb.DMatrix(data = as.matrix(xgb_test), label = xgb_test_dv)

#model prediction
xgbpred3 <- predict (model, xgb_test_DMatrix)





# Predict outcome on test data
xgbpred_prob3 <- predict (model, xgb_test_DMatrix, type = 'prob')
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

games <- x_test |> left_join(full_data) |> mutate(home_win_predict = xgb_test_dv_fac3) |>
         select(week,away_team,home_team,home_win_predict,home_beat_spread)

