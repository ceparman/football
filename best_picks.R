good_picks <- (xgbpred_val < .25 | xgbpred_val > .75)

y_pred_num3 <- as.factor(ifelse(xgbpred_val[good_picks] > 0.5, "YES", "NO"))

xgb_test_dv_fac3<- as.factor(ifelse(xgb_val_dv[good_picks]  == 1, "YES", "NO"))

# Print Confusion matrix, & F1 score
cmval <-confusionMatrix(as.factor(y_pred_num3), as.factor(xgb_test_dv_fac3), positive = "YES")
cmval


thresh<-data.frame(Actual = y_val[good_picks], Prob = xgbpred_val[good_picks])


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
