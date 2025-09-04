# 디렉토리 지정
setwd("C:/Users/newld/OneDrive/Desktop/Li-ion Battery Prediction Based on FDA/Dataset/Interpolated Data/Temperature")

# 라이브러리 불러오기
library(fda)
library(fda.usc)
library(fdasrvf)
library(splines)
library(ggplot2)
library(tidyr)
library(dplyr)
library(MFPCA)
library(funData)

# 배터리 모니터링 데이터 불러오기
B0005_soh <- read.csv('B0005_soh.csv')
B0005_soh <- B0005_soh$SOH
B0005_current <- read.csv('B0005_current.csv')
B0005_temperature <- read.csv('B0005_temperature.csv')
B0005_voltage <- read.csv('B0005_voltage.csv')

B0006_soh <- read.csv('B0006_soh.csv')
B0006_soh <- B0006_soh$SOH
B0006_current <- read.csv('B0006_current.csv')
B0006_temperature <- read.csv('B0006_temperature.csv')
B0006_voltage <- read.csv('B0006_voltage.csv')

B0007_soh <- read.csv('B0007_soh.csv')
B0007_soh <- B0007_soh$SOH
B0007_current <- read.csv('B0007_current.csv')
B0007_temperature <- read.csv('B0007_temperature.csv')
B0007_voltage <- read.csv('B0007_voltage.csv')

B0018_soh <- read.csv('B0018_soh.csv')
B0018_soh <- B0018_soh$SOH
B0018_current <- read.csv('B0018_current.csv')
B0018_temperature <- read.csv('B0018_temperature.csv')
B0018_voltage <- read.csv('B0018_voltage.csv')




#-------------------------------------------------------------------------------
predict_fpcr <- function(fdata_mat, y, start_train_size, type_) {
  n_cycles <- ncol(fdata_mat)
  preds <- c()
  trues <- c()
  best_k <- c()
  t_start <- proc.time()[["elapsed"]]  # 시작 시간(초)
  for (i in start_train_size:(n_cycles - 1)) {
    # Train data
    
    train_x <- as.matrix(fdata_mat[, 1:i])
    train_y <- y[1:i]
    
    # Test data
    test_x <- as.matrix(fdata_mat[, i + 1])
    test_y <- y[i + 1]
    
    # Fit model
    model <- elastic.pcr.regression.cv(train_x, train_y,  time = seq(1,371), pca.method = type_,
                                       no = 25,smooth_data = T, sparam = 10)
    # Predict
    pred<- predict(model$pca, newdata = test_x)
    pred <- pred %*% model$b + model$alpha
    
    # Store results
    preds <- c(preds, pred)
    trues <- c(trues, test_y)
    best_k <- c(best_k, model$best_k)
    
    # Progress print
    cat(sprintf("Cycle %d → %d | True: %.4f | Predicted: %.4f | best_k: %s\n",
                i, i + 1, test_y, pred, paste(model$best_k, collapse=",")))
  }
  
  # RMSE 계산
  rmse_val <- sqrt(mean((preds - trues)^2))
  cat(sprintf("Final RMSE: %.4f\n", rmse_val))
  
  # 총 소요 시간(초)
  total_elapsed_sec <- proc.time()[["elapsed"]] - t_start
  cat(sprintf("Total elapsed time: %.3f seconds\n", total_elapsed_sec))
  
  return(list(
    preds = preds,
    trues = trues,
    rmse = rmse_val,
    best_k = best_k,
    total_time_sec = as.numeric(total_elapsed_sec)
  ))
}
#-------------------------------------------------------------------------------
B0005_tmp_combined <- predict_fpcr(B0005_temperature, B0005_soh, start_train_size = 100, type_ = "combined")
write.csv(B0005_tmp_combined, "B0005_tmp_combined.csv")

B0005_tmp_vertical <- predict_fpcr(B0005_temperature, B0005_soh, start_train_size = 100, type_ = "vert")
write.csv(B0005_tmp_vertical, "B0005_tmp_vertical.csv")

B0005_tmp_horizontal <- predict_fpcr(B0005_temperature, B0005_soh, start_train_size = 100, "horiz")
write.csv(B0005_tmp_horizontal, "B0005_tmp_horizontal.csv")

B0005_vol_combined <- predict_fpcr(B0005_voltage, B0005_soh, start_train_size = 100, "combined")
write.csv(B0005_vol_combined, "B0005_vol_combined.csv")

B0005_vol_vertical <- predict_fpcr(B0005_voltage, B0005_soh, start_train_size = 100, "vert")
write.csv(B0005_vol_vertical, "B0005_vol_vertical.csv")

B0005_vol_horizontal <- predict_fpcr(B0005_voltage, B0005_soh, start_train_size = 100, "horiz")
write.csv(B0005_vol_horizontal, "B0005_vol_horizontal.csv")
#-------------------------------------------------------------------------------
B0006_tmp_combined <- predict_fpcr(B0006_temperature, B0006_soh, start_train_size = 100, "combined")
write.csv(B0006_tmp_combined, "B0006_tmp_combined.csv")

B0006_tmp_vertical <- predict_fpcr(B0006_temperature, B0006_soh, start_train_size = 100, "vert")
write.csv(B0006_tmp_vertical, "B0006_tmp_vertical.csv")

B0006_tmp_horizontal <- predict_fpcr(B0006_temperature, B0006_soh, start_train_size = 100, "horiz")
write.csv(B0006_tmp_horizontal, "B0006_tmp_horizontal.csv")

B0006_vol_combined <- predict_fpcr(B0006_voltage, B0006_soh, start_train_size = 100, "combined")
write.csv(B0006_vol_combined, "B0006_vol_combined.csv")

B0006_vol_vertical <- predict_fpcr(B0006_voltage, B0006_soh, start_train_size = 100, "vert")
write.csv(B0006_vol_vertical, "B0006_vol_vertical.csv")

B0006_vol_horizontal <- predict_fpcr(B0006_voltage, B0006_soh, start_train_size = 100, "horiz")
write.csv(B0006_vol_horizontal, "B0006_vol_horizontal.csv")

#-------------------------------------------------------------------------------
B0007_tmp_combined <- predict_fpcr(B0007_temperature, B0007_soh, start_train_size = 100, "combined")
 write.csv(B0007_tmp_combined, "B0007_tmp_combined.csv")

B0007_tmp_vertical <- predict_fpcr(B0007_temperature, B0007_soh, start_train_size = 100, "vert")
write.csv(B0007_tmp_vertical, "B0007_tmp_vertical.csv")

B0007_tmp_horizontal <- predict_fpcr(B0007_temperature, B0007_soh, start_train_size = 100, "horiz")
write.csv(B0007_tmp_horizontal, "B0007_tmp_horizontal.csv")

B0007_vol_combined <- predict_fpcr(B0007_voltage, B0007_soh, start_train_size = 100, "combined")
write.csv(B0007_vol_combined, "B0007_vol_combined.csv")

B0007_vol_vertical <- predict_fpcr(B0007_voltage, B0007_soh, start_train_size = 100, "vert")
write.csv(B0007_vol_vertical, "B0007_vol_vertical.csv")

B0007_vol_horizontal <- predict_fpcr(B0007_voltage, B0007_soh, start_train_size = 100, "horiz")
write.csv(B0007_vol_horizontal, "B0007_vol_horizontal.csv")

#-------------------------------------------------------------------------------
B0018_tmp_combined <- predict_fpcr(B0018_temperature, B0018_soh, start_train_size = 80, "combined")
write.csv(B0018_tmp_combined, "B0018_tmp_combined.csv")

B0018_tmp_vertical <- predict_fpcr(B0018_temperature, B0018_soh, start_train_size = 80, "vert")
write.csv(B0018_tmp_vertical, "B0018_tmp_vertical.csv")

B0018_tmp_horizontal <- predict_fpcr(B0018_temperature, B0018_soh, start_train_size = 80, "horiz")
write.csv(B0018_tmp_horizontal, "B0018_tmp_horizontal.csv")

B0018_vol_combined <- predict_fpcr(B0018_voltage, B0018_soh, start_train_size = 80, "combined")
write.csv(B0018_vol_combined, "B0018_vol_combined.csv")

B0018_vol_vertical <- predict_fpcr(B0018_voltage, B0018_soh, start_train_size = 80, "vert")
write.csv(B0018_vol_vertical, "B0018_vol_vertical.csv")

B0018_vol_horizontal <- predict_fpcr(B0018_voltage, B0018_soh, start_train_size = 80, "horiz")
write.csv(B0018_vol_horizontal, "B0018_vol_horizontal.csv")
  
#-------------------------------------------------------------------------------
rmse <- function(actual, pred) {sqrt(mean((actual - pred)^2, na.rm = TRUE))}
mae <- function(actual, pred) {mean(abs(actual - pred), na.rm = TRUE)}
abort
rmse(B0005_tmp_combined$preds, B0005_tmp_combined$trues)
rmse(B0005_tmp_vertical$preds, B0005_tmp_vertical$trues)
rmse(B0005_tmp_horizontal$preds, B0005_tmp_horizontal$trues)
rmse(B0005_vol_combined$preds, B0005_vol_combined$trues)
rmse(B0005_vol_vertical$preds, B0005_vol_combined$trues)
rmse(B0005_vol_horizontal$preds, B0005_vol_combined$trues)

rmse(B0006_tmp_combined$preds, B0006_tmp_combined$trues)
rmse(B0006_tmp_vertical$preds, B0006_tmp_vertical$trues)
rmse(B0006_tmp_horizontal$preds, B0006_tmp_horizontal$trues)
rmse(B0006_vol_combined$preds, B0006_vol_combined$trues)
rmse(B0006_vol_vertical$preds, B0006_vol_vertical$trues)
rmse(B0006_vol_horizontal$preds, B0006_vol_horizontal$trues)

rmse(B0007_tmp_combined$preds, B0007_tmp_combined$trues)
rmse(B0007_tmp_vertical$preds, B0007_tmp_vertical$trues)
rmse(B0007_tmp_horizontal$preds, B0007_tmp_horizontal$trues)
rmse(B0007_vol_combined$preds, B0007_vol_combined$trues)
rmse(B0007_vol_vertical$preds, B0007_vol_vertical$trues)
rmse(B0007_vol_horizontal$preds, B0007_vol_horizontal$trues)

#-------------------------------------------------------------------------------

model1 <- elastic.pcr.regression.cv(train, train_y, time = seq(1:371), no = 25)


train_pred <- predict(model1)
plot(train_y, type = "l")
lines(train_pred$y_pred, col = "blue")

pred <- predict(model1$pca, newdata = test)
pred <- pred %*% model1$b + model1$alpha

plot(test_y, type = "l", ylim = c(0.7,1))
lines(pred, col = "blue")

B0007_tmp_combined$total_time_sec

write.csv(B0007_tmp_combined$preds,"B0007_tmp_combined.csv")
