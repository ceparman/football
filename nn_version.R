library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(xgboost)
library(SHAPforxgboost)
library(zoo)
library(caret)
library(neuralnet)



#load model data

data_files <- list.files("model_data/",full.names = T)

full_data <- data.frame()

for(file in data_files) {
  
  year_data <- readRDS(file)
  
  full_data <- rbind(full_data,year_data )
  
}

full_data <- full_data |>  mutate_if(is.character, as.factor) |> select((where(is.numeric))) |>
                     na.omit()


set.seed(245)
data_rows <- floor(0.80 * nrow(full_data))
train_indices <- sample(c(1:nrow(full_data)), data_rows)
train_data <- full_data[train_indices,]
test_data <- full_data[-train_indices,]


model = neuralnet(
  home_beat_spread ~ .,
  data=train_data,
  hidden=c(4,2),
  linear.output = FALSE,
  lifesign = "minimal"
)


pred <- data.frame( predict = (predict(model, test_data) > .5) ,
                    actual = (test_data$home_beat_spread == 1)
)
                    
  table(pred$predict,pred$actual)

  sum(pred$predict==pred$actual)/nrow(pred) *100

