library(tidymodels)
tidymodels_prefer()



#load modeling data

data_files <- list.files("model_data/",full.names = T)

full_data <- data.frame()

for(file in data_files) {
  
  year_data <- readRDS(file)
  
  full_data <- rbind(full_data,year_data )
  
}


metric <- "home_beat_spread"


#only use up too the last year 

model_data <- full_data |>    filter( season < max(season)) |> select(-c(game_result,season,game_id,home_team,week,away_team)) 

val_data <- full_data |>  filter( season == max(season)) |> select(-c(game_result,season,game_id,home_team,week,away_team)) 

#split data


set.seed(502)
model_split <- initial_split(model_data, prop = 0.80, strata = metric)
model_train <- training(model_split)
model_test  <-  testing(model_split)



forest_model <- rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()


 forest_xy_fit <- 
   forest_model %>% 
  fit_xy(
    x = model_train %>% select(-home_beat_spread),
    y = model_train %>% pull(home_beat_spread)
  )

model_pred <- predict(forest_xy_fit, new_data = model_test %>% select(-home_beat_spread)) 
 
test_truth<- as.factor(ifelse( model_test |> pull(home_beat_spread) == 1, "YES", "NO"))
test_pred<- as.factor(ifelse(model_pred > .5, "YES", "NO"))

model_tp <-data.frame(truth=test_truth,predict = test_pred)

conf_mat <-confusionMatrix(test_truth,test_pred, positive = "YES")

conf_mat                         


 conf_mat(model_tp, truth = truth, estimate = predict)

 
 
 model_train$home_beat_spread <- as.factor(model_train$home_beat_spread )
 
 
 
 
 xgb_spec <-boost_tree(
   trees = 500,
   tree_depth = tune(), 
   min_n = tune(),
   loss_reduction = tune(),                    ## first three: model complexity
   sample_size = tune(), mtry = tune(),        ## randomness
   learn_rate = tune()                         ## step size
 ) %>%
   set_engine("xgboost") %>%
   set_mode("classification")
 xgb_spec
 
 
 
 xgb_rec <- recipe(home_beat_spread ~., data = model_train)
 
 xgb_wf <- workflow() %>%
   add_formula(home_beat_spread ~.) %>%
   add_model(xgb_spec)
 xgb_wf
 
 
 xgb_grid <- grid_latin_hypercube(
   tree_depth(),
   min_n(),
   loss_reduction(),
   sample_size = sample_prop(),
   finalize(mtry(),  model_train),
   learn_rate(),
   size = 2
 )
 
 library(finetune)

 
 folds <- vfold_cv(model_train, strata = home_beat_spread)
 
 set.seed(234)
 xgb_res <-tune_grid(
   xgb_wf,
   resamples = folds,
   grid = xgb_grid,
   control = control_grid(save_pred  = TRUE)
 )
 xgb_res
 
 show_best(xgb_res,metric =  "roc_auc")
 
 best_auc <- select_best(xgb_res,,metric = "roc_auc")
 
 final_xgb <- finalize_workflow(xgb_wf, best_auc)
 final_xgb
 
 
 library(vip)
 final_xgb %>%
   fit(data = model_train) %>%
   extract_fit_parsnip() %>%
   vip(geom = "point")

 final_rs <- last_fit(final_xgb, model_split,
                      metrics = metric_set(accuracy, roc_auc, sens,spec))
 final_rs %>%
   collect_metrics()
 final_rs %>%
   collect_predictions() %>%
   conf_mat(home_beat_spread, .pred_class) 

  