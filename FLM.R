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

basis_x = seq(5,50, by = 1)
basis_b = seq(5,50, by = 1)
lambda_ = seq(0,1, by = 0.1)
predict_fregre <- function(fdata_mat, y, start_train_size) {
  t_start <- proc.time()[["elapsed"]]  # 시작 시간(초)
  n_cycles <- ncol(fdata_mat)
  preds <- c()
  trues <- c()
  
  for (i in start_train_size:(n_cycles - 1)) {
    # Train data
    
    train_x <- fdata(t(fdata_mat[, 1:i]))
    train_y <- y[1:i]
    
    # Test data
    test_x <- fdata(t(fdata_mat[, i + 1, drop = FALSE]))
    test_y <- y[i + 1]
    
    # Fit model
    model <- fregre.basis.cv(train_x, train_y,
                             basis.x = basis_x, basis.b = basis_b,
                             lambda = lambda_)
    
    # Predict
    pred <- predict(model$fregre.basis, test_x)
    
    # Store results
    preds <- c(preds, pred)
    trues <- c(trues, test_y)
    
    # Progress print
    cat(sprintf("Cycle %d → %d | True: %.4f | Predicted: %.4f\n",
                i, i + 1, test_y, pred))
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
    total_time_sec = as.numeric(total_elapsed_sec)
  ))
}


B0005_tmp_flm <- predict_fregre(B0005_temperature, B0005_soh, start_train_size = 100)
write.csv(B0005_tmp_flm, "B0005_tmp_flm.csv")

B0005_vol_flm <- predict_fregre(B0005_voltage, B0005_soh, start_train_size = 100)
write.csv(B0005_vol_flm, "B0005_vol_flm.csv")

B0006_tmp_flm <- predict_fregre(B0006_temperature, B0006_soh, start_train_size = 100)
write.csv(B0006_tmp_flm, "B0006_tmp_flm.csv")

B0006_vol_flm <- predict_fregre(B0006_voltage, B0006_soh, start_train_size = 100)
write.csv(B0006_vol_flm, "B0006_vol_flm.csv")

B0007_tmp_flm <- predict_fregre(B0007_temperature, B0007_soh, start_train_size = 100)
write.csv(B0007_tmp_flm, "B0007_tmp_flm.csv")

B0007_vol_flm <- predict_fregre(B0007_voltage, B0007_soh, start_train_size = 100)
write.csv(B0007_vol_flm, "B0007_vol_flm.csv")

B0018_tmp_flm <- predict_fregre(B0018_temperature, B0018_soh, start_train_size = 80)
write.csv(B0018_tmp_flm, "B0018_tmp_flm.csv")

B0018_vol_flm <- predict_fregre(B0018_voltage, B0018_soh, start_train_size = 80)
write.csv(B0018_vol_flm, "B0018_vol_flm.csv")
  



















