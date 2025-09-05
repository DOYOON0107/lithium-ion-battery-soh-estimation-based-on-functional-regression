# import library
setwd("C:/Users/newld/OneDrive/Desktop/Lithium-Ion-Battery-Prediction/[0] Data Preprocessing/BatteryData/Battery[5, 6, 7, 18]")
library(fda)
library(fda.usc)
library(fdasrvf)
library(splines)
library(ggplot2)
library(tidyr)
library(dplyr)

# import dataset
B0005 <- read.csv("B0005_discharge.csv")
B0006 <- read.csv("B0006_discharge.csv")
B0007 <- read.csv("B0007_discharge.csv")
B0018 <- read.csv("B0018_discharge.csv")

B0005_cycle <- length(unique(B0005$cycle))
B0006_cycle <- length(unique(B0006$cycle))
B0007_cycle <- length(unique(B0007$cycle))
B0018_cycle <- length(unique(B0018$cycle))


# Draw plots of voltage, temperature 
plot(x = B0018$time[B0018$cycle == 1],
     y = B0018$voltage_measured[B0018$cycle == 1], type = "l",
     xlab = "time", ylab = "voltage", main = "B0018")

for (i in 2:B0018_cycle){
  lines(x = B0018$time[B0018$cycle == i],
        y = B0018$voltage_measured[B0018$cycle == i],
        col = i %% 10)}

plot(x = B0018$time[B0018$cycle == 1],
     y = B0018$temperature_measured[B0018$cycle == 1], type = "l",
     xlab = "time", ylab = "temperature", ylim = c(25, 45), main = "B0018")

for (i in 2:B0018_cycle){
  lines(x = B0018$time[B0018$cycle == i],
        y = B0018$temperature_measured[B0018$cycle == i],
        col = i %% 10)}